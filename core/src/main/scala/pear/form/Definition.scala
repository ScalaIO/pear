package pear
package form

import scala.language.higherKinds
import scala.language.implicitConversions
import matryoshka.{CorecursiveT, Delay}
import scalaz.{Applicative, Cord, Functor, Kleisli, Show, Traverse, \/}

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object Definition {

  type Form = matryoshka.data.Fix[FormF]

  sealed trait FormF[A] extends Product with Serializable

  final case class Optional[A](form: A)                         extends FormF[A]
  final case class Fields[A](fields: Vector[(String, A)])       extends FormF[A]
  final case class Choice[A](alternatives: Vector[(String, A)]) extends FormF[A]
  final case class Sequence[A](element: A)                      extends FormF[A]
  final case class Value[A](constraint: Constraint)             extends FormF[A]

  implicit def formFTraverse[L]: Traverse[FormF[?]] = new Traverse[FormF[?]] {
    import scalaz.std.vector.vectorInstance
    import scalaz.syntax.traverse._

    def traverseImpl[G[_], A, B](fa: FormF[A])(f: A => G[B])(implicit G: Applicative[G]): G[FormF[B]] = fa match {
      case Optional(v) => Functor[G].map(f(v))(Optional.apply)
      case Fields(fs) =>
        val (names, values) = fs.unzip
        Functor[G].map(values traverse f)(vs => Fields(names zip vs))
      case Choice(alts) =>
        val (names, values) = alts.unzip
        Functor[G].map(values traverse f)(as => Choice(names zip as))
      case Sequence(elem) =>
        Functor[G].map(f(elem))(Sequence.apply)
      case Value(c) => Applicative[G].point(Value[B](c))
    }
  }

  implicit def formShow[L](implicit showC: Show[Constraint]) = new Delay[Show, FormF[?]] {
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
        case Sequence(elem) =>
          showA.show(elem) ++ Cord("*")
        case Value(c) =>
          showC.show(c)
      }
    }
  }

  private def parse[O](unsafeParse: String => form.FormValue): form.Constraint =
    Kleisli[\/[String, ?], String, FormValue]((s: String) =>
      \/.fromTryCatchNonFatal(unsafeParse(s)).leftMap(e => s"For input string $s, got ${e.getClass}: ${e.getMessage}"))

  def optional[T[_[_]], L](form: T[FormF])(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Optional(form))
  def mapping[T[_[_]], L](fields: (String, T[FormF])*)(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Fields(fields.toVector))
  def choice[T[_[_]], L](alternatives: (String, T[FormF])*)(implicit T: CorecursiveT[T]): T[FormF] =
    T.embedT[FormF](Choice(alternatives.toVector))

  def int: Constraint = parse(s => ValueNum(s.toInt))

  def isoDateTime = parse(s => ValueDate(ZonedDateTime.parse(s, DateTimeFormatter.ISO_DATE_TIME)))

  implicit def lift[T[_[_]]](f: Constraint)(implicit T: CorecursiveT[T]): T[Definition.FormF] =
    T.embedT[Definition.FormF](Definition.Value(f))

}
