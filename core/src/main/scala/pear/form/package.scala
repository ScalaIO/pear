package pear

import scala.language.higherKinds

import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz._
import Scalaz._
import java.net.URLDecoder
import scala.util.{Try, Success, Failure}

package form {

  case class Path(elements: Vector[String]) extends AnyVal {
    def ++(other: Path)           = Path(elements ++ other.elements)
    def /(elem: String)           = Path(elements :+ elem)
    def isChildOf(other: Path)    = (this != other) && elements.startsWith(other.elements)
    def relativeTo(other: Path)   = Path(elements.drop(other.elements.size))
    override def toString: String = elements.mkString(".")
  }

  object Path {
    def empty                       = Path(Vector.empty)
    def apply(string: String): Path = Path(string.split('.').toVector)
  }

  final case class DecodedForm(map: Map[Path, String]) extends AnyVal {
    def valueAt(path: Path): Option[String] = map.get(path)
    def getList(path: Path): Vector[Path] = {
      map.keys
        .filter(_.isChildOf(path))
        .map(_.relativeTo(path))
        .filter(_.elements.head.forall(_.isDigit))
        .map(path / _.elements.head)
        .toVector
    }
  }

  final case class EvaluationContext[T[_[_]]](form: DecodedForm,
                                              path: Path,
                                              result: T[EnvT[NonEmptyList[Error] \/ FormValue, Definition.FormF, ?]]) {
    def lookup: Option[String] = form.valueAt(path)
  }

  object EvaluationContext {
    def init[T[_[_]]](input: DecodedForm)(implicit T: CorecursiveT[T]): EvaluationContext[T] =
      EvaluationContext(input, Path.empty, T.embedT[form.Validated](EnvT((\/-(ValueNull), Definition.Empty()))))
  }

}

package object form {
  import Definition._

  import java.time.ZonedDateTime
  import java.time.format.DateTimeFormatter

  type Outcome = NonEmptyList[Error] \/ FormValue

  type Validated[A] = EnvT[Outcome, FormF, A]

  type Validator[T[_[_]]] = EvaluationContext[T] => EvaluationContext[T]

  implicit class ParsingOps(input: String) {
    def parseFormUrlEncoded: DecodedForm =
      DecodedForm(
        input
          .split("&")
          .filter(_.count(_ == '=') == 1)
          .map { s =>
            val Array(key, value) = s.split("=")
            Path(URLDecoder.decode(key, "UTF-8")) -> URLDecoder.decode(value, "UTF-8")
          }
          .toMap
      )
  }

  implicit class FormOps[T[_[_]]](f: T[FormF])(implicit T: BirecursiveT[T]) {
    def validate(value: String): T[Validated] = {
      val input     = value.parseFormUrlEncoded
      val validator = makeValidator(f)
      validator(EvaluationContext.init(input)).result
    }
  }

  implicit class ValidatedOps[T[_[_]]](v: T[Validated])(implicit T: BirecursiveT[T]) {
    def outcome: Outcome = T.projectT[Validated](v).ask
  }

  def result[T[_[_]]](outcome: NonEmptyList[Error] \/ FormValue, ctr: FormF[T[Validated]], ctx: EvaluationContext[T])(
      implicit T: CorecursiveT[T]) =
    ctx.copy(result = T.embedT[Validated](EnvT((outcome, ctr))))

  def missingValue[T[_[_]]](ctx: EvaluationContext[T], f: FormF[T[Validated]])(implicit T: CorecursiveT[T]) =
    error("missing.value", f, ctx)

  def error[T[_[_]]](msg: String, ctr: FormF[T[Validated]], ctx: EvaluationContext[T])(implicit T: CorecursiveT[T]) =
    ctx.copy(result = T.embedT[Validated](EnvT((NonEmptyList.nels(Error(ctx.path, msg)).left, ctr))))

  def success[T[_[_]]](value: FormValue, ctr: FormF[T[Validated]], ctx: EvaluationContext[T])(
      implicit T: CorecursiveT[T]) =
    ctx.copy(result = T.embedT[Validated](EnvT((value.right, ctr))))

  def evaluate[T[_[_]]](implicit T: BirecursiveT[T]): Algebra[FormF, Validator[T]] = {
    case Empty() => identity[EvaluationContext[T]]
    case Optional(v) => { ctx =>
      val EvaluationContext(_, _, res) = v(ctx)
      val outcome = res.outcome.fold(
        _ => \/-(ValueNull),
        valid => valid.right
      )
      result(outcome, Optional(res), ctx)
    }
    case Fields(fields) => { ctx =>
      val subs = fields.map { case (k, v) => k -> v(ctx.copy(path = ctx.path / k)).result }
      val decorations =
        subs
          .map { case (k, e) => T.projectT[Validated](e).ask.map(f => Vector(k -> f)) }
          .suml
          .map(v => ValueObject(v.toMap))
      val tree: FormF[T[Validated]] = Fields(subs)
      result(decorations, tree, ctx)
    }
    case Choice(alternatives) => { ctx =>
      ctx.lookup
        .flatMap { selection =>
          alternatives.toMap
            .get(selection)
            .map { validator =>
              validator(ctx.copy(path = Path(selection)))
            }
        }
        .getOrElse {
          val rebuiltChoices = alternatives.map { case (k, v) => k -> v(ctx).result }
          missingValue(ctx, Choice(rebuiltChoices))
        }
    }
    case Sequence(elems) => { ctx =>
      val paths = ctx.form.getList(ctx.path)
      val subEnvs =
        paths.map(path => T.projectT[Validated](elems.head(ctx.copy(path = path)).result))
      val elements = subEnvs.map(_.ask.map(fv => Vector(fv))).suml.map(elems => ValueList(elems))
      result(elements, Sequence(subEnvs.map(T.embedT[Validated])), ctx)
    }
    case Number() =>
      tryParse(str => ValueNum(BigDecimal(str)), Number(), "malformed.number")
    case IsoDateTime() =>
      tryParse(str => ValueDate(ZonedDateTime.parse(str, DateTimeFormatter.ISO_DATE_TIME)),
               IsoDateTime(),
               "malfored.date")
    case MinLength(min) =>
      checkPredicate(MinLength(min)) {
        case (ctx, valid @ ValueStr(n)) if n.length >= min => success(valid, MinLength(min), ctx)
        case (ctx, ValueStr(_))                            => error("min.length.not.met", MinLength(min), ctx)
      }
    case MaxLength(max) =>
      checkPredicate(MaxLength(max)) {
        case (ctx, valid @ ValueStr(n)) if n.length < max => success(valid, MaxLength(max), ctx)
        case (ctx, ValueStr(_))                           => error("max.length.exceeded", MaxLength(max), ctx)
      }
    case Min(min) =>
      checkPredicate(Min(min)) {
        case (ctx, valid @ ValueNum(n)) if n >= min => success(valid, Min(min), ctx)
        case (ctx, ValueNum(_))                     => error("min.value.not.met", Min(min), ctx)
      }
    case Max(max) =>
      checkPredicate(Max(max)) {
        case (ctx, valid @ ValueNum(n)) if n < max => success(valid, Max(max), ctx)
        case (ctx, ValueNum(_))                    => error("max.value.exceeded", Max(max), ctx)
      }
    case After(start) =>
      checkPredicate(After(start)) {
        case (ctx, valid @ ValueDate(d)) if d.isAfter(start) =>
          success(valid, After(start), ctx)
        case (ctx, ValueDate(_)) => error("too.soon", After(start), ctx)
      }
    case Before(end) =>
      checkPredicate(Before(end)) {
        case (ctx, valid @ ValueDate(d)) if d.isBefore(end) =>
          success(valid, Before(end), ctx)
        case (ctx, ValueDate(_)) => error("too.late", Before(end), ctx)
      }
    case AndThen(lhs, rhs) => { ctx =>
      rhs(lhs(ctx))
    }
  }

  def checkPredicate[T[_[_]]](ctr: => FormF[T[Validated]])(
      predicate: PartialFunction[(EvaluationContext[T], FormValue), EvaluationContext[T]])(
      implicit T: BirecursiveT[T]): Validator[T] = { ctx =>
    ctx.result.outcome.fold(
      errs => ctx.copy(result = T.embedT[Validated](EnvT((-\/(errs), ctr)))),
      valid =>
        if (predicate.isDefinedAt((ctx, valid))) predicate((ctx, valid))
        else error("incoherent.definition", ctr, ctx)
    )
  }

  def tryParse[T[_[_]]: CorecursiveT, O <: FormValue](attempt: String => O,
                                                      ctr: FormF[T[Validated]],
                                                      errMsg: String): Validator[T] = { ctx =>
    ctx.lookup
      .map { str =>
        Try(attempt(str)) match {
          case Success(v) => success(v, ctr, ctx)
          case Failure(_) => error(errMsg, ctr, ctx)
        }
      }
      .getOrElse {
        missingValue(ctx, ctr)
      }

  }

  def makeValidator[T[_[_]]](form: T[FormF])(implicit T: BirecursiveT[T]): Validator[T] =
    form.cata(evaluate)

}
