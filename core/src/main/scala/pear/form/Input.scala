package pear
package form

import matryoshka.RecursiveT
import scala.language.higherKinds

sealed trait Input extends Product with Serializable {
  def path: Path
}

object Input {

  def getValue(input: Input, at: Path): Option[String] = input match {
    case FormObject(_, v) => v.get(at).map(_.toString)
    case FormList(_, v)   => v.get(at).map(_.toString)
    case _                => None
  }

  def getField(input: Input, at: Path): Input = input match {
    case FormObject(path, v) => v.get(at).map(Field(path ++ at, _)).getOrElse(NoValue(path ++ at))
    case x                   => NoValue(x.path ++ at)
  }

  def getForm(input: Input, at: Path): FormObject = {
    def extractForm(path: Path, v: Map[Path, UrlEncoded]) =
      FormObject(path ++ at, v.collect {
        case (key, value) if key.startsWith(at) =>
          key.diff(at) -> value
      })

    input match {

      case FormObject(path, v) => extractForm(path, v)
      case FormList(path, v)   => extractForm(path, v)
      case x                   => FormObject(x.path ++ at, Map.empty)
    }
  }

  def getList(input: Input, at: Path): FormList = input match {
    case FormObject(path, v) =>
      FormList(
        path ++ at,
        v.collect {
          case (key, value) if key.startsWith(at) && key.diff(at).headOption.exists(_.forall(_.isDigit)) =>
            key.diff(at) -> value
        }
      )
    case x => FormList(x.path ++ at, Map.empty)
  }

  // TODO implement that using a proper recursion scheme rather than with explicit recursion
  def getAt[T[_[_]]](input: Input, pos: Path, schema: T[Definition.FormF])(implicit T: RecursiveT[T]): Input =
    T.projectT[Definition.FormF](schema) match {
      case Definition.Fields(_) =>
        getForm(input, pos)
      case Definition.Value(_) => getField(input, pos)
      case Definition.Sequence(_) =>
        getList(input, pos)
      case Definition.Choice(alts) =>
        (for {
          choice <- getValue(input, pos)
          select <- alts.toMap.get(choice)
        } yield getAt(input, choice, select)).getOrElse(NoValue(input.path ++ pos))
      case Definition.Optional(x) =>
        getAt(input, pos, x)

    }

  def getAt[T[_[_]]](input: Input, pos: String, schema: T[Definition.FormF])(implicit T: RecursiveT[T]): Input =
    getAt(input, Path(pos), schema)

}

final case class FormObject(path: Path, v: Map[Path, UrlEncoded])    extends Input
final case class FormList(path: Path, values: Map[Path, UrlEncoded]) extends Input
final case class NoValue(path: Path)                                 extends Input
final case class Field(path: Path, value: UrlEncoded)                extends Input

final case class UrlEncoded(str: String) {
  import java.net.URLDecoder
  override def toString: String = URLDecoder.decode(str, "UTF-8")
}
