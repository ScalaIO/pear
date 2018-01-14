package pear
package form

import org.scalatest.{EitherValues, WordSpec, Matchers}
import scalaz.\/-
import scalaz.syntax.compose._

class ValidationSpec extends WordSpec with Matchers with EitherValues {

  import matryoshka.data.Fix, Fix._
  import Definition._

  val singleInt: Form = mapping("foo" -> int)

  "singleInt" should {
    "accept 'foo=42'" in {
      val result = singleInt.validate("foo=42").unFix
      result.ask.toEither should be('right)
      result.ask should be(\/-(ValueObject(Map("foo" -> ValueNum(42)))))

    }

    "reject 'foo=bar'" in {
      singleInt.validate("foo=bar").unFix.ask.toEither should be('left)
    }

    "reject 'bar=42'" in {
      singleInt.validate("bar=42").unFix.ask.toEither should be('left)
    }
  }

  val optionalSingleInt: Fix[FormF] = optional(singleInt)

  "optionalSingleInt" should {
    "accept empty form" in {
      optionalSingleInt.validate("").unFix.ask should be(\/-(ValueNull))
    }
  }

  val singleOptionalInt: Form = mapping("foo" -> optional(int))

  "singleOptionalInt" should {
    "be tolerant with invalid values" in {
      singleOptionalInt.validate("foo=bar").unFix.ask should be(\/-(ValueObject(Map("foo" -> ValueNull))))
    }
  }

  val form: Fix[Definition.FormF] = mapping(
    "foo" -> optional(andThen(int, Fix(Min(0)))),
    "bar" -> mapping(
      "qux" -> int,
      "baz" -> optional(int)
    )
  )

  "form" should {
    "accept 'foo=42&bar.qux=12'" in {
      val result = form.validate("foo=42&bar.qux=12").unFix.ask
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

  val list: Fix[Definition.FormF] = mapping("things" -> sequence(form))

  "list" should {
    "accept 'things.0.foo=42&things.0.bar.qux=12&things.1.foo=83&things.1.bar.baz=1&things.1.bar.qux=0'" in {
      val result =
        list
          .validate("things.0.foo=42&things.0.bar.qux=12&things.1.foo=83&things.1.bar.baz=1&things.1.bar.qux=0")
          .unFix
          .ask
      result should be(
        \/-(
          ValueObject(
            Map(
              "things" -> ValueList(Vector(
                ValueObject(Map("foo" -> ValueNum(42),
                                "bar" -> ValueObject(Map(
                                  "qux" -> ValueNum(12),
                                  "baz" -> ValueNull
                                )))),
                ValueObject(Map("foo" -> ValueNum(83),
                                "bar" -> ValueObject(Map(
                                  "qux" -> ValueNum(0),
                                  "baz" -> ValueNum(1)
                                ))))
              ))
            ))
        )
      )

    }
  }

  val alt: Fix[Definition.FormF] = mapping("choose" -> choice("form" -> form, "otherwise" -> int))

  "alt" should {
    "accept 'choose=form&form.foo=42&form.bar.qux=12'" in {
      val result = alt.validate("choose=form&form.foo=42&form.bar.qux=12").unFix.ask
      result should be(
        \/-(
          ValueObject(
            Map(
              "choose" -> ValueObject(
                Map("foo" -> ValueNum(42),
                    "bar" -> ValueObject(Map(
                      "qux" -> ValueNum(12),
                      "baz" -> ValueNull
                    ))))
            ))
        ))
    }

    "accept 'choose=otherwise&otherwise=24'" in {
      val result = alt.validate("choose=otherwise&otherwise=24").unFix.ask
      result should be(
        \/-(
          ValueObject(
            Map(
              "choose" -> ValueNum(24)
            ))
        )
      )
    }
  }
}
