package pear

import scala.language.higherKinds

import matryoshka.{AlgebraM, BirecursiveT, CoalgebraM}
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz.{\/, IList, Kleisli, NonEmptyList, State}
import scalaz.syntax.either._

package object form {

  type Path          = Vector[String]
  type Zipped[A]     = EnvT[Decoration, Validation.FormF, A]
  type Seed[T[_[_]]] = (Decoration, T[Definition.FormF])
  type Errors[A]     = State[List[List[Error]], A]
  type Constraint    = Kleisli[\/[String, ?], String, FormValue]

  implicit class ParsingOps(input: String) {
    def parseFormUrlEncoded: Map[String, UrlEncoded] =
      input
        .split("&")
        .filter(_.count(_ == '=') == 1)
        .map { s =>
          val Array(key, value) = s.split("=")
          key -> new UrlEncoded(value)
        }
        .toMap
  }

  implicit class FormOps[T[_[_]]](f: T[Definition.FormF])(implicit T: BirecursiveT[T]) {
    def validate(value: String): NonEmptyList[List[Error]] \/ FormValue = {
      val (errors, result) = zip((FormObject(Vector.empty[String], value.parseFormUrlEncoded), f)).run(Nil)
      if (errors.isEmpty) result.right
      else NonEmptyList.nel(errors.head, IList.fromList(errors.tail)).left
    }
  }

  def zipWithValue[T[_[_]]](implicit T: BirecursiveT[T]): CoalgebraM[Errors, Zipped, Seed[T]] = {
    case (v, t) =>
      (v, T.projectT[Definition.FormF](t)) match {
        case (f @ FormObject(_, _), Definition.Fields(fs)) =>
          val subs: Vector[(String, Seed[T])] = fs.map {
            case (key, subForm) =>
              key -> (f.getAt(key, subForm) -> subForm)
          }
          State(errors => (errors, EnvT((v, Validation.Fields(subs)))))
        case (d, Definition.Value(c)) =>
          State(errors => (errors, EnvT((d, Validation.Value(c)))))
        case (d, Definition.Optional(x)) =>
          State(errors => (errors, EnvT((d, Validation.Optional((d, x))))))
        case (FormList(path, values), Definition.Sequence(elem)) => ??? // TODO
        case (d, x) =>
          State(
            errors =>
              (List(Error(d.path, s"error while zipping, ($d, $x) is unexpected")) :: errors,
               EnvT((d, Validation.Erroneous()))))
      }
  }

  def evaluate[T[_[_]]]: AlgebraM[Errors, Zipped, FormValue] = { env =>
    (env.ask, env.lower) match {
      case (_, Validation.Fields(f)) =>
        State(errors => (errors, ValueObject(f.toMap)))
      case (FormList(p, _), Validation.Sequence(l)) =>
        State(errors => (errors, ValueList(l.toList)))
      case (Field(p, s), Validation.Value(c)) =>
        c.run(s.toString)
          .fold(
            err => State(errors => (List(Error(p, err)) :: errors, ValueNull)),
            value => State(errors => (errors, value))
          )
      case (NoValue(p), Validation.Value(_)) =>
        State(errors => (List(Error(p, s"Missing mandatory value at path $p")) :: errors, ValueNull))
      case (_, Validation.Optional(ValueNull)) =>
        State(errors => (errors.tail, ValueNull))
      case (_, Validation.Optional(x)) =>
        State(
          errors =>
            if (errors.isEmpty) (Nil, x)
            else (errors.tail, ValueNull))
      case (_, Validation.Erroneous()) =>
        State(errors => (errors, ValueNull))
      case (s, v) =>
        State(errors => (List(Error(s.path, s"incoherent decoration for value : ($s, $v)")) :: errors, ValueNull))
    }
  }

  def zip[T[_[_]]](seed: Seed[T])(implicit T: BirecursiveT[T]) =
    seed.hyloM[Errors, Zipped, FormValue](evaluate[T], zipWithValue[T])

}
