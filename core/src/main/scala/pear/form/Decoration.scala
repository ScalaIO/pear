package pear
package form

import scala.language.higherKinds

import matryoshka.BirecursiveT

sealed trait Decoration extends Product with Serializable {
  def path: Path
}

final case class FormObject(path: Path, v: Map[String, UrlEncoded]) extends Decoration {

  def getValue(field: String): Option[String] = v.get(field).map(_.toString)

  def getField(field: String) = v.get(field).map(Field(path :+ field, _)).getOrElse(NoValue(path :+ field))

  def getForm(field: String): FormObject =
    FormObject(path :+ field, v.collect {
      case (key, value) if key.startsWith(s"$field.") =>
        key.substring(field.length() + 1) -> value
    })

  def getList(field: String): FormList =
    FormList(
      path :+ field,
      v.collect {
        case (key, value) if key.startsWith(s"$field.") && key.substring(field.length + 1).forall(_.isDigit) =>
          key.substring(field.length + 1) -> value
      }
    )

  // TODO implement that using a proper recursion scheme rather than with explicit recursion
  def getAt[T[_[_]]](pos: String, schema: T[Definition.FormF])(implicit T: BirecursiveT[T]): Decoration =
    T.projectT[Definition.FormF](schema) match {
      case Definition.Fields(_) =>
        getForm(pos)
      case Definition.Value(_)    => getField(pos)
      case Definition.Sequence(_) => getList(pos)
      case Definition.Choice(alts) =>
        (for {
          choice <- getValue(pos)
          select <- alts.toMap.get(choice)
        } yield getAt(choice, select)).getOrElse(NoValue(path :+ pos))
      case Definition.Optional(x) =>
        getAt(pos, x)

    }
}

final case class FormList(path: Path, values: Map[String, UrlEncoded]) extends Decoration
final case class NoValue(path: Path)                                   extends Decoration
final case class Field(path: Path, value: UrlEncoded)                  extends Decoration

final case class UrlEncoded(str: String) {
  import java.net.URLDecoder
  override def toString: String = URLDecoder.decode(str, "UTF-8")
}
