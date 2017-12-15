package pear
package form

import scala.language.higherKinds

import scalaz.{Functor, Traverse, Applicative, Show, Cord}
import matryoshka.Delay

object Validation {

  sealed trait FormF[A] extends Product with Serializable

  final case class Optional[A](inner: A)                  extends FormF[A]
  final case class Fields[A](fields: Vector[(String, A)]) extends FormF[A]
  final case class Sequence[A](element: Vector[A])        extends FormF[A]
  final case class Value[A](constraint: Constraint)       extends FormF[A]
  final case class Selection[A](inner: A)                 extends FormF[A]
  final case class Erroneous[A]()                         extends FormF[A]

  implicit def formFTraverse[L]: Traverse[FormF[?]] = new Traverse[FormF[?]] {
    import scalaz.std.vector.vectorInstance
    import scalaz.syntax.traverse._

    def traverseImpl[G[_], A, B](fa: FormF[A])(f: A => G[B])(implicit G: Applicative[G]): G[FormF[B]] = fa match {
      case Erroneous()  => Applicative[G].point(Erroneous())
      case Optional(i)  => Functor[G].map(f(i))(Optional.apply)
      case Selection(i) => Functor[G].map(f(i))(Selection.apply)
      case Fields(fs) =>
        val (names, values) = fs.unzip
        Functor[G].map(values traverse f)(vs => Fields(names zip vs))
      case Sequence(elements) =>
        Functor[G].map(elements traverse f)(Sequence.apply)
      case Value(c) => Applicative[G].point(Value[B](c))
    }
  }

  implicit def formShow[L](implicit showC: Show[Constraint]) = new Delay[Show, FormF] {
    def apply[A](showA: Show[A]): Show[FormF[A]] = new Show[FormF[A]] {
      override def show(f: FormF[A]): Cord = f match {
        case Erroneous()  => Cord("!!ERROR!!")
        case Optional(i)  => Cord("optional<") ++ showA.show(i) ++ Cord(">")
        case Selection(i) => showA.show(i)
        case Fields(fields) =>
          Cord("{ ") ++ Cord
            .mkCord(Cord(", "), fields.map { case (n, v) => Cord(s"$n : ") ++ showA.show(v) }: _*) ++ Cord(" }")
        case Sequence(elems) => Cord("[") ++ Cord.mkCord(Cord(", "), elems.map(showA.show): _*) ++ Cord("*")
        case Value(c)        => showC.show(c)
      }
    }
  }

}
