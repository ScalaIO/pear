package pear
package form

import scala.language.higherKinds

import matryoshka.BirecursiveT

sealed trait Decoration extends Product with Serializable {
  def path: Path
}

final case class FormObject(path: Path, v: Map[Path, UrlEncoded]) extends Decoration {

  def getValue(at: Path): Option[String] = v.get(at).map(_.toString)

  def getField(at: Path): Decoration = v.get(at).map(Field(path ++ at, _)).getOrElse(NoValue(path ++ at))

  def getForm(at: Path): FormObject =
    FormObject(path ++ at, v.collect {
      case (key, value) if key.startsWith(at) =>
        key.diff(at) -> value
    })

  def getList(at: Path): FormList =
    FormList(
      path ++ at,
      v.collect {
        case (key, value) if key.startsWith(at) && key.diff(at).mkString(".").forall(_.isDigit) =>
          key.diff(at) -> value
      }
    )

  // TODO implement that using a proper recursion scheme rather than with explicit recursion
  def getAt[T[_[_]]](pos: Path, schema: T[Definition.FormF])(implicit T: BirecursiveT[T]): Decoration =
    T.projectT[Definition.FormF](schema) match {
      case Definition.Fields(_) =>
        getForm(pos)
      case Definition.Value(_)    => getField(pos)
      case Definition.Sequence(_) => getList(pos)
      case Definition.Choice(alts) =>
        (for {
          choice <- getValue(pos)
          select <- alts.toMap.get(choice)
        } yield getAt(choice, select)).getOrElse(NoValue(path ++ pos))
      case Definition.Optional(x) =>
        getAt(pos, x)

    }

  def getAt[T[_[_]]](pos: String, schema: T[Definition.FormF])(implicit T: BirecursiveT[T]): Decoration =
    getAt(Path(pos), schema)

}

final case class FormList(path: Path, values: Map[Path, UrlEncoded]) extends Decoration
final case class NoValue(path: Path)                                 extends Decoration
final case class Field(path: Path, value: UrlEncoded)                extends Decoration

final case class UrlEncoded(str: String) {
  import java.net.URLDecoder
  override def toString: String = URLDecoder.decode(str, "UTF-8")
}
