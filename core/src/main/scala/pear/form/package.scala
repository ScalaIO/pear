package pear

import scala.language.higherKinds

import matryoshka.{AlgebraM, BirecursiveT, RecursiveT, CoalgebraM}
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz.{\/, IList, Kleisli, NonEmptyList, State}
import scalaz.syntax.either._

package object form {

  object Path {
    def apply(string: String): Path = string.split('.').toVector
  }
  type Path          = Vector[String]
  type Decorated[A]  = EnvT[Input, Validation.FormF, A]
  type Seed[T[_[_]]] = (Input, T[Definition.FormF])
  type Errors[A]     = State[List[List[Error]], A]
  type Constraint    = Kleisli[\/[String, ?], String, FormValue]

  implicit class ParsingOps(input: String) {
    def parseFormUrlEncoded: Map[Path, UrlEncoded] =
      input
        .split("&")
        .filter(_.count(_ == '=') == 1)
        .map { s =>
          val Array(key, value) = s.split("=")
          Path(key) -> UrlEncoded(value)
        }
        .toMap
  }

  implicit class FormOps[T[_[_]]](f: T[Definition.FormF])(implicit T: BirecursiveT[T]) {
    def validate(value: String): NonEmptyList[List[Error]] \/ FormValue = {
      val (errors, result) =
        validationTraversal((FormObject(Vector.empty[String], value.parseFormUrlEncoded), f)).run(Nil)
      if (errors.isEmpty) result.right
      else NonEmptyList.nel(errors.head, IList.fromList(errors.tail)).left
    }
  }

  /**
    * Starting from a Seed, eg. a pair (Input, T[Definition.FormF]), traverse the
    * definition top-down to produce a T[Validation.FormF] with each node decorated
    * with the relevant part of tje initial input.
    *
    * During that process, every Choice is determined according to the input and hence
    * does not appear in the resulting Validation.FormF.
    *
    * We abstract over T which can be any fix-point type, provided there is a RecursiveT
    * instance for it (that provides us with the projectT method that extract the F
    * from a T[F].
    */
  def decorate[T[_[_]]](implicit T: RecursiveT[T]): CoalgebraM[Errors, Decorated, Seed[T]] = {
    case a @ (v, t) =>
      (v, T.projectT[Definition.FormF](t)) match {
        case (f @ FormObject(_, _), Definition.Fields(fs)) =>
          val subs: Vector[(String, Seed[T])] = fs.map {
            case (key, subForm) =>
              key -> (Input.getAt(f, key, subForm) -> subForm)
          }
          State(errors => (errors, EnvT((v, Validation.Fields(subs)))))
        case (d, Definition.Value(c)) =>
          State(errors => (errors, EnvT((d, Validation.Value(c)))))
        case (d, Definition.Optional(x)) =>
          State(errors => (errors, EnvT((d, Validation.Optional((d, x))))))
        case (l @ FormList(path, values), Definition.Sequence(elem)) =>
          val elements = values.map { case (k, _) => (Input.getAt(l, Path(k.head), elem), elem) }.toVector
          State(errors => (errors, EnvT((l, Validation.Sequence(elements)))))
        case (d, x) =>
          State(
            errors =>
              (List(Error(d.path, s"error while zipping, ($d, $x) is unexpected")) :: errors,
               EnvT((d, Validation.Erroneous()))))
      }
  }

  def evaluate[T[_[_]]]: AlgebraM[Errors, Decorated, FormValue] = { env =>
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

  def validationTraversal[T[_[_]]](seed: Seed[T])(implicit T: BirecursiveT[T]): Errors[FormValue] =
    seed.hyloM[Errors, Decorated, FormValue](evaluate[T], decorate[T])

}
