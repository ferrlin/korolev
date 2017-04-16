package korolev.template

import scala.language.experimental.macros
import scala.language.implicitConversions

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class TemplateDsl[MiscType](val templateContext: TemplateContext[MiscType]) {

  import templateContext._
  import Document._

  def template(document: => Document): Template =
    macro TemplateMacro.toProgrammaticTemplate

  /** Converts () to empty template */
  implicit def unit(value: Unit): Document = Empty

  /** Converts [[String]] to text document node */
  implicit def text(value: String): Text = Text(value)

  /** Converts iterable of templates to document fragment */
  implicit def seq(xs: Iterable[NodeLike]): Fragment =
    Fragment(xs.toSeq)

  /** Implicitly unwraps optional documents */
  implicit def unwrapOption(templateOpt: Option[Document]): Document = {
    templateOpt match {
      case Some(value) => value
      case None => Empty
    }
  }

  /** Symbol based DSL allows to define documents
    * {{{
    *   'body(
    *     'h1(class /= "title", "Hello World"),
    *     'p("Lorem ipsum dolor")
    *   )
    * }}}
    */
  implicit final class SymbolOps(s: Symbol) {
    def apply(xs: Document*): Node = Node(s.name, xs)
    def /=(value: String): Attr = Attr(s.name, value)
  }

  @deprecated("Use () instead of <>", since = "0.4.0")
  val <> = Empty
}
