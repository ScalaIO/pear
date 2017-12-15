package pear

import scala.language.higherKinds

import matryoshka.{AlgebraM, BirecursiveT, RecursiveT, CoalgebraM}
import matryoshka.implicits._
import matryoshka.patterns.EnvT
import scalaz.{\/, IList, Kleisli, NonEmptyList, StateT, State, Id}
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
    * Starting from a Seed[T], eg. a pair (Path, T[Definition.FormF]), traverse the
    * definition top-down to produce a T[Validation.FormF] with each node decorated
    * with the path of the corresponding value.
    *
    * During that process, every Choice is determined according to the input.
    *
    * We abstract over T which can be any fix-point type, provided there is a RecursiveT
    * instance for it (that provides us with the projectT method that extract the F
    * from a T[F]).
    */
  def decorate[T[_[_]]](input: DecodedForm)(implicit T: RecursiveT[T]): CoalgebraM[Errors, Decorated, Seed[T]] = {
    case a @ (v, t) =>
      (v, T.projectT[Definition.FormF](t)) match {
        case (path, Definition.Fields(fs)) =>
          // Decorate each field with a path built by concatenating this Fields' path
          // and the field's name
          val subs: Vector[(String, Seed[T])] = fs.map {
            case (key, subForm) =>
              key -> (path / key -> subForm)
          }
          StateT.stateT(EnvT((v, Validation.Fields(subs))))
        case (path, Definition.Value(c)) =>
          // Nothing much to do, just translate to Validation.Value
          StateT.stateT(EnvT((path, Validation.Value(c))))
        case (path, Definition.Optional(x)) =>
          // Optional is just a "marker" layer, so we just push the same path
          // down to the inner form
          StateT.stateT(EnvT((path, Validation.Optional(path -> x))))
        case (path, Definition.Sequence(elem)) =>
          // Here we need to expand a Definition.Sequence that only contains the
          // *schema* of an element to a Validation.Sequence that contains the
          // actual *instances* of this schema.
          // So we basically end up copying [[elem]] as many times as there are elements
          // for this sequence in the input.
          //
          // NOTE: the astute reader would have noticed that we only compute the size
          // of the input list, without verifying that there is an element for every
          // index. That is OK because if there is no value for a given index, this will
          // be caught by the [[evaluate]] algebra.
          StateT.stateT(
            EnvT(
              (path, Validation.Sequence((0 to input.listSize(path)).map(i => (path / i.toString) -> elem).toVector))))
        case (path, Definition.Choice(alt)) =>
          // This is somehow the dual of the Sequence case. Definition.Choice contains
          // several alternatives, but we need to choose only one to construct a
          // Validation.Selection
          (for {
            selection  <- input.valueAt(path)
            definition <- alt.toMap.get(selection)
          } yield
            StateT.stateT[Id.Id, List[List[Error]], Decorated[Seed[T]]]( // type inference sucks!
              EnvT(path -> Validation.Selection(Path(selection) -> definition))))
            .getOrElse(
              State(errors => (List(Error(path, "invalid choice")) :: errors, EnvT((path, Validation.Erroneous()))))
            )

      }
  }

  /**
    * Starting from a T[Decorated], wich is basically a T[Validation.FormF] where each
    * layer has a Path attached to it, construct a FormValue by tearing it down bottom up.
    * Along the way, accumulates errors in a State monad.
    */
  def evaluate[T[_[_]]](input: DecodedForm): AlgebraM[Errors, Decorated, FormValue] = { env =>
    (env.ask, env.lower) match {
      case (_, Validation.Fields(f)) =>
        // Nothing more to do than wrapping the f:Vector[(String, FormValue)]
        // into a ValueObject
        StateT.stateT(ValueObject(f.toMap))
      case (_, Validation.Sequence(l)) =>
        // Nothing more to do than wrapping the f:Vector[FormValue]
        // into a ValueList
        StateT.stateT(ValueList(l.toList))
      case (path, Validation.Value(c)) =>
        // Here comes the real validation. We did all the above fiddling with path
        // just so we know here how to grab a value from the input.
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
        // Something obviously went wrong at the lower layer
        // but since Optional means that we tolerate null values
        // we just have to remove that error from the stack
        State(errors => (errors.drop(1), ValueNull))
      case (path, Validation.Optional(x)) =>
        // Here we don't know yet if anything went wrong one layer bellow
        State { errors =>
          // So let's try and remove errors that happened one level bellow
          val prunedErrors = errors.filterNot(_.forall(_.path.isChildOf(path)))
          // If there was any, just remove them and return ValueNull
          if (prunedErrors.size < errors.size) (prunedErrors, ValueNull)
          // Otherwise, everything was fine in the first place!
          else (errors, x)
        }
      case (_, Validation.Erroneous()) =>
        // An Erroneous node was emmited with an error during the
        // top-down phase, nothing more to do here
        StateT.stateT(ValueNull)
      case (_, Validation.Selection(r)) =>
        // Selection is only there so that the structure of Validation.FormF matches the 
        // one of Definition.FormF enough so that we don't need to look at multiple
        // layers of Definition during de top-down phase, so here we just need to 
        // peel it off.
        StateT.stateT(r)
    }
  }

  def validationTraversal[T[_[_]]](seed: Seed[T], input: DecodedForm)(implicit T: BirecursiveT[T]): Errors[FormValue] =
    seed.hyloM[Errors, Decorated, FormValue](evaluate[T](input), decorate[T](input))

}
