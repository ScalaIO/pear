package pear
package form

import org.scalatest.{EitherValues, WordSpec, Matchers}
import scalaz.\/-
import scalaz.syntax.either._

class ValidationSpec extends WordSpec with Matchers with EitherValues {

  import matryoshka.data.Fix, Fix._
  import Definition._

  val singleInt: Form = mapping("foo" -> int)

  "singleInt" should {
    "accept 'foo=42'" in {
      val result = singleInt.validate("foo=42")
      result.toEither should be('right)
      result should be(\/-(ValueObject(Map("foo" -> ValueNum(42)))))

    }

    "reject 'foo=bar'" in {
      singleInt.validate("foo=bar").toEither should be('left)
    }

    "reject 'bar=42'" in {
      singleInt.validate("bar=42").toEither should be('left)
    }
  }

  val optionalSingleInt: Fix[FormF] = optional(singleInt)

  "optionalSingleInt" should {
    "accept empty form" in {
      optionalSingleInt.validate("") should be(\/-(ValueNull))
    }
  }

  val singleOptionalInt: Form = mapping("foo" -> optional(int))

  "singleOptionalInt" should {
    "be tolerant with invalid values" in {
      singleOptionalInt.validate("foo=bar") should be(\/-(ValueObject(Map("foo" -> ValueNull))))
    }
  }

  val form: Fix[Definition.FormF] = mapping(
    "foo" -> optional(int >==> { i =>
      val ValueNum(n) = i
      if (n >= 0) i.right else "must be positive".left
    }),
    "bar" -> mapping(
      "qux" -> int,
      "baz" -> optional(int)
    )
  )

  "form" should {
    "accept 'foo=42&bar.qux=12'" in {
      val result = form.validate("foo=42&bar.qux=12")
      result should be(
        \/-(
          ValueObject(
            Map("foo" -> ValueNum(42),
                "bar" -> ValueObject(Map(
                  "qux" -> ValueNum(12),
                  "baz" -> ValueNull
                ))))))
    }
  }
}
