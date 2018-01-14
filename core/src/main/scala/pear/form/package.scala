package pear

import scala.language.higherKinds

import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz._
import Scalaz._
import java.net.URLDecoder

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
                                              result: T[EnvT[NonEmptyList[Error] \/ FormValue, Definition.FormF, ?]])

  object EvaluationContext {
    def init[T[_[_]]](input: DecodedForm)(implicit T: CorecursiveT[T]): EvaluationContext[T] =
      EvaluationContext(input, Path.empty, T.embedT[form.Validated](EnvT((\/-(ValueNull), Definition.Empty()))))
  }

}
package object form {
  import Definition._

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
      val input = value.parseFormUrlEncoded
      val validator =
        validationTraversal(f)
      validator(EvaluationContext.init(input)).result
    }
  }

  type Validated[A] = EnvT[\/[NonEmptyList[Error], FormValue], FormF, A]

  type Validator[T[_[_]]] = EvaluationContext[T] => EvaluationContext[T]

  import java.time.ZonedDateTime
  import java.time.format.DateTimeFormatter

  @inline def terminal[T[_[_]]](f: FormF[T[Validated]]): FormF[T[Validated]] = f
  @inline def missingValue[T[_[_]]](path: Path, f: FormF[T[Validated]])(implicit T: CorecursiveT[T]): T[Validated] =
    T.embedT[Validated](EnvT((-\/(NonEmptyList.nels(Error(path, "missing.value"))), terminal(f))))

  def evaluate[T[_[_]]](T: BirecursiveT[T]): Algebra[FormF, Validator[T]] = {
    case Empty() => { ctx =>
      ctx
    }
    case Optional(v) => { ctx =>
      val EvaluationContext(form, _, res) = v(ctx)
      ctx.copy(
        result = T.embedT[Validated](
          EnvT((T.projectT[Validated](res).ask.fold(_ => \/-(ValueNull), identity(_).right), Optional(res)))))
    }
    case Fields(fields) => { ctx =>
      val subs = fields.map { case (k, v) => k -> v(ctx.copy(path = ctx.path / k)).result }
      val decorations =
        subs
          .map { case (k, e) => T.projectT[Validated](e).ask.map(f => Vector(k -> f)) }
          .suml
          .map(v => ValueObject(v.toMap))
      val tree: FormF[T[Validated]] = Fields(subs)
      ctx.copy(result = T.embedT[Validated](EnvT((decorations, tree))))
    }
    case Choice(alternatives) => { ctx =>
      ctx.form
        .valueAt(ctx.path)
        .flatMap { selection =>
          alternatives.toMap
            .get(selection)
            .map { validator =>
              validator(ctx.copy(path = Path(selection)))
            }
        }
        .getOrElse {
          val rebuiltChoices = alternatives.map { case (k, v) => k -> v(ctx).result }
          ctx.copy(result = missingValue(ctx.path, Choice(rebuiltChoices))(T))
        }
    }
    case Sequence(elems) => { ctx =>
      val paths = ctx.form.getList(ctx.path)
      val subEnvs =
        paths.map(path => T.projectT[Validated](elems.head(ctx.copy(path = path)).result))
      val elements = subEnvs.map(_.ask.map(fv => Vector(fv))).suml.map(elems => ValueList(elems))
      EvaluationContext(ctx.form,
                        ctx.path,
                        T.embedT[Validated](EnvT((elements, Sequence(subEnvs.map(T.embedT[Validated]))))))
    }
    case Number() => { ctx =>
      val result = ctx.form
        .valueAt(ctx.path)
        .map { (str: String) =>
          val result = \/.fromTryCatchNonFatal(ValueNum(BigDecimal(str))).leftMap(_ =>
            NonEmptyList.nels(Error(ctx.path, "malformed.number")))
          T.embedT[Validated](EnvT((result, terminal(Number()))))
        }
        .getOrElse {
          missingValue(ctx.path, Number())(T)
        }
      ctx.copy(result = result)
    }
    case IsoDateTime() => { ctx =>
      val result = ctx.form
        .valueAt(ctx.path)
        .map { str =>
          val result = \/.fromTryCatchNonFatal(ValueDate(ZonedDateTime.parse(str, DateTimeFormatter.ISO_DATE_TIME)))
            .leftMap(_ => NonEmptyList.nels(Error(ctx.path, "malformed.date")))
          T.embedT[Validated](EnvT((result, terminal(IsoDateTime()))))
        }
        .getOrElse {
          missingValue(ctx.path, IsoDateTime())(T)
        }
      EvaluationContext(ctx.form, ctx.path, result)
    }
    case MinLength(min) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(MinLength(min))))))
        case valid @ \/-(ValueStr(s)) if s.length >= min =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(MinLength(min))))))
        case \/-(ValueStr(_)) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "min.length.not.met")).left, terminal(MinLength(min))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(MinLength(min))))))
      }
    }
    case MaxLength(max) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(MaxLength(max))))))
        case valid @ \/-(ValueStr(s)) if s.length < max =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(MaxLength(max))))))
        case \/-(ValueStr(_)) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "max.length.exceeded")).left, terminal(MaxLength(max))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(MaxLength(max))))))
      }
    }
    case Min(min) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(Min(min))))))
        case valid @ \/-(ValueNum(s)) if s >= min =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(Min(min))))))
        case \/-(ValueNum(_)) =>
          EvaluationContext(ctx.form,
                            ctx.path,
                            T.embedT[Validated](
                              EnvT((NonEmptyList.nels(Error(ctx.path, "min.value.not.met")).left, terminal(Min(min))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(Min(min))))))
      }
    }
    case Max(max) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(Max(max))))))
        case valid @ \/-(ValueNum(s)) if s < max =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(Max(max))))))
        case \/-(ValueNum(_)) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "max.value.exceeded")).left, terminal(Max(max))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(Max(max))))))
      }
    }
    case After(start) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(After(start))))))
        case valid @ \/-(ValueDate(s)) if s.isAfter(start) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(After(start))))))
        case \/-(ValueDate(_)) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](EnvT((NonEmptyList.nels(Error(ctx.path, "too.soon")).left, terminal(After(start))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(After(start))))))
      }
    }
    case Before(end) => { ctx =>
      T.projectT[Validated](ctx.result).ask match {
        case errs @ -\/(_) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((errs, terminal(Before(end))))))
        case valid @ \/-(ValueDate(s)) if s.isBefore(end) =>
          EvaluationContext(ctx.form, ctx.path, T.embedT[Validated](EnvT((valid, terminal(Before(end))))))
        case \/-(ValueDate(_)) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](EnvT((NonEmptyList.nels(Error(ctx.path, "too.late")).left, terminal(Before(end))))))
        case \/-(x) =>
          EvaluationContext(
            ctx.form,
            ctx.path,
            T.embedT[Validated](
              EnvT((NonEmptyList.nels(Error(ctx.path, "incoherent.definition")).left, terminal(Before(end))))))
      }
    }
    case AndThen(lhs, rhs) => { ctx =>
      rhs(lhs(ctx))
    }
  }

  def validationTraversal[T[_[_]]](form: T[FormF])(implicit T: BirecursiveT[T]): Validator[T] =
    form.cata(evaluate(T))

}
