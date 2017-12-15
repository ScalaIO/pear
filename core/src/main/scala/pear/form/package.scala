package pear

import scala.language.higherKinds

import matryoshka.{AlgebraM, BirecursiveT, RecursiveT, CoalgebraM}
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz.{\/, IList, Kleisli, NonEmptyList, StateT, State}
import scalaz.syntax.either._
import java.net.URLDecoder

package form {

  case class Path(elements: Vector[String]) extends AnyVal {
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
    def listSize(path: Path): Int = {
      map.keys
        .filter(_.isChildOf(path))
        .map(_.relativeTo(path))
        .map(_.elements.head)
        .filter(_.forall(_.isDigit))
        .map(_.toInt)
        .max
    }
  }
}
package object form {

  type Decorated[A]  = EnvT[Path, Validation.FormF, A]
  type Seed[T[_[_]]] = (Path, T[Definition.FormF])
  type Errors[A]     = State[List[List[Error]], A]
  type Constraint    = Kleisli[\/[String, ?], String, FormValue]

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

  implicit class FormOps[T[_[_]]](f: T[Definition.FormF])(implicit T: BirecursiveT[T]) {
    def validate(value: String): NonEmptyList[List[Error]] \/ FormValue = {
      val (errors, result) =
        validationTraversal((Path.empty, f), value.parseFormUrlEncoded).run(Nil)
      if (errors.isEmpty) result.right
      else NonEmptyList.nel(errors.head, IList.fromList(errors.tail)).left
    }
  }

  /**
    * Starting from a Seed, eg. a pair (Path, T[Definition.FormF]), traverse the
    * definition top-down to produce a T[Validation.FormF] with each node decorated
    * with the path of the corresponding value.
    *
    * During that process, every Choice is determined according to the input. 
    *
    * We abstract over T which can be any fix-point type, provided there is a RecursiveT
    * instance for it (that provides us with the projectT method that extract the F
    * from a T[F].
    */
  def decorate[T[_[_]]](input: DecodedForm)(implicit T: RecursiveT[T]): CoalgebraM[Errors, Decorated, Seed[T]] = {
    case a @ (v, t) =>
      (v, T.projectT[Definition.FormF](t)) match {
        case (path, Definition.Fields(fs)) =>
          val subs: Vector[(String, Seed[T])] = fs.map {
            case (key, subForm) =>
              key -> (path / key -> subForm)
          }
          StateT.stateT(EnvT((v, Validation.Fields(subs))))
        case (path, Definition.Value(c)) =>
          StateT.stateT(EnvT((path, Validation.Value(c))))
        case (path, Definition.Optional(x)) =>
          StateT.stateT(EnvT((path, Validation.Optional((path, x)))))
        case (path, Definition.Sequence(elem)) =>
          StateT.stateT(
            EnvT(
              (path, Validation.Sequence((0 to input.listSize(path)).map(i => (path / i.toString) -> elem).toVector))))
        case (path, Definition.Choice(alt)) =>
          input.valueAt(path).flatMap(sel => alt.toMap.get(sel).map(sel -> _)) match {
            case Some((selection, definition)) =>
              StateT.stateT(EnvT(path -> Validation.Selection(Path(selection) -> definition)))
            case None =>
              State(errors => (List(Error(path, "invalid choice")) :: errors, EnvT((path, Validation.Erroneous()))))
          }

      }
  }

  def evaluate[T[_[_]]](input: DecodedForm): AlgebraM[Errors, Decorated, FormValue] = { env =>
    (env.ask, env.lower) match {
      case (_, Validation.Fields(f)) =>
        StateT.stateT(ValueObject(f.toMap))
      case (_, Validation.Sequence(l)) =>
        StateT.stateT(ValueList(l.toList))
      case (path, Validation.Value(c)) =>
        input.valueAt(path) match {
          case None =>
            State(errors => (List(Error(path, "missing mandatory value")) :: errors, ValueNull))
          case Some(s) =>
            c.run(s)
              .fold(
                err => State(errors => (List(Error(path, err)) :: errors, ValueNull)),
                value => State(errors => (errors, value))
              )
        }
      case (_, Validation.Optional(ValueNull)) =>
        State(errors => (errors.tail, ValueNull))
      case (_, Validation.Optional(x)) =>
        State(
          errors =>
            if (errors.isEmpty) (Nil, x)
            else (errors.tail, ValueNull))
      case (_, Validation.Erroneous()) =>
        StateT.stateT(ValueNull)
      case (_, Validation.Selection(r)) =>
        StateT.stateT(r)
    }
  }

  def validationTraversal[T[_[_]]](seed: Seed[T], input: DecodedForm)(implicit T: BirecursiveT[T]): Errors[FormValue] =
    seed.hyloM[Errors, Decorated, FormValue](evaluate[T](input), decorate[T](input))

}
