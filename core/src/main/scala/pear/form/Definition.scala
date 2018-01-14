package pear
package form

import java.time.ZonedDateTime
import scala.language.higherKinds

import matryoshka.{CorecursiveT, Delay}
import scalaz.{Applicative, Cord, Show, Traverse}

object Definition {

  type Form = matryoshka.data.Fix[FormF]

  sealed trait FormF[A]                                         extends Product with Serializable
  final case class Empty[A]()                                   extends FormF[A]
  final case class Optional[A](form: A)                         extends FormF[A]
  final case class Fields[A](fields: Vector[(String, A)])       extends FormF[A]
  final case class Choice[A](alternatives: Vector[(String, A)]) extends FormF[A]
  final case class Sequence[A](element: Vector[A])              extends FormF[A]
  final case class Number[A]()                                  extends FormF[A]
  final case class IsoDateTime[A]()                             extends FormF[A]
  final case class MinLength[A](minLength: Int)                 extends FormF[A]
  final case class MaxLength[A](maxLength: Int)                 extends FormF[A]
  final case class Min[A](min: BigDecimal)                      extends FormF[A]
  final case class Max[A](max: BigDecimal)                      extends FormF[A]
  final case class After[A](start: ZonedDateTime)               extends FormF[A]
  final case class Before[A](end: ZonedDateTime)                extends FormF[A]
  final case class AndThen[A](lhs: A, rhs: A)                   extends FormF[A]

  implicit def formFTraverse[L]: Traverse[FormF] = new Traverse[FormF] {
    import scalaz.std.vector.vectorInstance
    import scalaz.syntax.traverse._

    def traverseImpl[G[_], A, B](fa: FormF[A])(f: A => G[B])(implicit G: Applicative[G]): G[FormF[B]] = fa match {
      case Empty()     => G.point(Empty[B]())
      case Optional(v) => G.map(f(v))(Optional.apply)
      case Fields(fs) =>
        val (names, values) = fs.unzip
        G.map(values traverse f)(vs => Fields(names zip vs))
      case Choice(alts) =>
        val (names, values) = alts.unzip
        G.map(values traverse f)(as => Choice(names zip as))
      case Sequence(elems)   => G.map(elems traverse f)(Sequence.apply)
      case Number()          => G.point(Number[B]())
      case IsoDateTime()     => G.point(IsoDateTime[B]())
      case MinLength(min)    => G.point(MinLength[B](min))
      case MaxLength(max)    => G.point(MaxLength[B](max))
      case Min(min)          => G.point(Min[B](min))
      case Max(max)          => G.point(Max[B](max))
      case After(start)      => G.point(After[B](start))
      case Before(end)       => G.point(Before[B](end))
      case AndThen(lhs, rhs) => G.apply2(f(lhs), f(rhs))(AndThen.apply)
    }
  }

  implicit def formShow: Delay[Show, FormF] = new Delay[Show, FormF] {
    def apply[A](showA: Show[A]): Show[FormF[A]] = new Show[FormF[A]] {
      override def show(f: FormF[A]): Cord = f match {
        case Optional(a) =>
          Cord("optional(") ++ showA.show(a) ++ Cord(")")
        case Fields(fields) =>
          Cord("{ ") ++ Cord
            .mkCord(Cord(", "), fields.map { case (n, v) => Cord(s"$n : ") ++ showA.show(v) }: _*) ++ Cord(" }")
        case Choice(alt) =>
          Cord("[ ") ++ Cord.mkCord(Cord(" | "), alt.map { case (k, v) => Cord(s"$k:") ++ showA.show(v) }: _*) ++ Cord(
            " ]")
        case Sequence(elems) =>
          Cord.mkCord(Cord(", "), elems.map(showA.show): _*) ++ Cord("*")
        case terminal => Cord(terminal.toString)
      }
    }
  }

  def optional[T[_[_]], L](form: T[FormF])(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Optional(form))
  def mapping[T[_[_]], L](fields: (String, T[FormF])*)(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Fields(fields.toVector))
  def choice[T[_[_]], L](alternatives: (String, T[FormF])*)(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Choice(alternatives.toVector))
  def sequence[T[_[_]]](element: T[FormF])(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Sequence(Vector(element)))

  def int[T[_[_]]](implicit T: CorecursiveT[T]) = T.embedT[FormF](Number())

  def isoDateTime[T[_[_]]](implicit T: CorecursiveT[T]) = T.embedT[FormF](IsoDateTime())

  def andThen[T[_[_]]](lhs: T[FormF], rhs: T[FormF])(implicit T: CorecursiveT[T]) = T.embedT[FormF](AndThen(lhs, rhs))
}
