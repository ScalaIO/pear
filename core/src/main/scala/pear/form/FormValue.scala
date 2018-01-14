package pear
package form

import java.time.ZonedDateTime

sealed trait FormValue                                       extends Product with Serializable
final case class ValueObject(fields: Map[String, FormValue]) extends FormValue
final case class ValueList(elements: Vector[FormValue])      extends FormValue
final case class ValueStr(value: String)                     extends FormValue
final case class ValueNum(value: BigDecimal)                 extends FormValue
final case class ValueDate(value: ZonedDateTime)             extends FormValue
final case class ValueBool(value: Boolean)                   extends FormValue
case object ValueNull                                        extends FormValue

object FormValue {
  def empty: FormValue = ValueNull
}

final case class Error(path: Path, message: String) {
  override def toString: String = s"$path: $message"
}
