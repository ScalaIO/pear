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
  import scala.annotation.switch

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

  private def vectorPairToJson(vec: Vector[(String, String)]): String =
    vec.map { case (k, v) => s""""$k":$v""" }.mkString(",")

  def toJsonAlg: Algebra[FormF, String] = {
    case Empty()              => "{}"
    case Optional(a)          => s"""{"optional":$a}"""
    case Fields(fields)       => s"""{"fields":{${vectorPairToJson(fields)}}}"""
    case Choice(alternatives) => s"""{"choice":{${vectorPairToJson(alternatives)}}}"""
    case Sequence(element)    => s"""{"sequence":$element}"""
    case Number()             => s"""{"number":{}}"""
    case IsoDateTime()        => s"""{"isoDateTime":{}}"""
    case MinLength(min)       => s"""{"minLength":$min}"""
    case MaxLength(max)       => s"""{"maxLength":$max}"""
    case Min(min)             => s"""{"min":$min}"""
    case Max(max)             => s"""{"max":$max}"""
    case After(start)         => s"""{"after":$start}"""
    case Before(end)          => s"""{"before":$end}"""
    case AndThen(lhs, rhs)    => s"""{"andThen":{"lhs":$lhs,"rhs":$rhs}}"""
  }

  def toJson[T[_[_]]](form: T[FormF])(implicit ev: RecursiveT[T]) =
    form.cata(toJsonAlg)

  import io.circe._

  def objectOnly[A](f: JsonObject => String \/ A) = new Json.Folder[String \/ A] {
    def onNull                    = "expected.object.found.null".left
    def onBoolean(x: Boolean)     = s"expected.object.found.boolean.$x".left
    def onNumber(x: JsonNumber)   = s"expected.object.found.number.$x".left
    def onString(x: String)       = s"expected.object.found.string.$x".left
    def onArray(x: Vector[Json])  = s"expected.object.found.array.$x".left
    def onObject(obj: JsonObject) = f(obj)
  }

  def fromJsonCoalg: CoalgebraM[String \/ ?, FormF, Json] = { json =>
    json
      .foldWith(objectOnly { json =>
        (json.size: @switch) match {
          case 1 =>
            json.keys.head match {
              case "optional" =>
                json("optional").map(Optional(_).right).getOrElse("incoherent.optional".left)
              case "fields" =>
                json("fields")
                  .map(_.foldWith(objectOnly(j => Fields(j.toVector).right)))
                  .getOrElse("incoherent.fields".left)
              case "choice" =>
                json("choice")
                  .map(_.foldWith(objectOnly(j => Choice(j.toVector).right)))
                  .getOrElse("incoherent.choice".left)
              case "sequence" =>
                json("sequence").map(j => Sequence(Vector(j)).right).getOrElse("incoherent.sequence".left)
              case "number" =>
                json("number")
                  .map(_.foldWith(objectOnly(j =>
                    if (j.size == 0) Number[Json]().right else "number.should.be.empty".left)))
                  .getOrElse("incoherent.number".left)
              case "isoDateTime" =>
                json("isoDateTime")
                  .map(
                    _.foldWith(objectOnly(j => if (j.size == 0) Number[Json]().right else "date.should.be.empty".left)))
                  .getOrElse("incoherent.date".left)
              case "minLength" =>
                json("minLength")
                  .flatMap(_.asNumber.flatMap(_.toInt))
                  .map(MinLength[Json](_).right)
                  .getOrElse("minlength.should.be.a.number".left)

              case "maxLength" =>
                json("maxLength")
                  .flatMap(_.asNumber.flatMap(_.toInt))
                  .map(MaxLength[Json](_).right)
                  .getOrElse("maxlength.should.be.a.number".left)
              case "min" =>
                json("min")
                  .flatMap(_.asNumber.flatMap(_.toInt))
                  .map(Min[Json](_).right)
                  .getOrElse("min.should.be.a.number".left)

              case "max" =>
                json("max")
                  .flatMap(_.asNumber.flatMap(_.toInt))
                  .map(Max[Json](_).right)
                  .getOrElse("max.should.be.a.number".left)

              case "before" =>
                json("before")
                  .flatMap(_.asString)
                  .map(str =>
                    \/.fromTryCatchNonFatal(Before[Json](ZonedDateTime.parse(str))).leftMap(_ => "unparsable.date"))
                  .getOrElse("start.should.be.a.string".left)

              case "after" =>
                json("after")
                  .flatMap(_.asString)
                  .map(str =>
                    \/.fromTryCatchNonFatal(After[Json](ZonedDateTime.parse(str))).leftMap(_ => "unparsable.date"))
                  .getOrElse("end.should.be.a.string".left)

              case "andThen" =>
                json("andThen")
                  .map(_.foldWith(objectOnly(j =>
                    (j("lhs") |@| j("rhs")) {
                      case (lhs, rhs) =>
                        AndThen(lhs, rhs).right
                    }.getOrElse("andThen.should.have.lhs.and.rhs".left))))
                  .getOrElse("malformed.andThen".left)

              case x => s"unkown.type.$x".left
            }
          case 0 => Empty[Json]().right
          case _ => "incoherent.definition".left
        }
      })

  }

}
