package korolev.template

import korolev.template.util.ToKebabCase

final class TemplateContext[MiscType] {

  sealed trait Document

  object Document {
    sealed trait NodeLike extends Document
    /** HTML tag. Like `<div></div>` */
    case class Node(name: String, children: Seq[Document]) extends NodeLike {
      /** Converts node to template */
      def toTemplate(implicit context: RenderContext): Template = {
        // expand converts any document to
        // template if it possible
        def expand(template: Document): Unit = template match {
          case pt: Template => pt.run()
          case node: Node => node.toTemplate(context).run()
          case misc: Misc => context.addMisc(misc)
          case Attr(n, value) => context.setAttr(toKebabCase(n), value)
          case Text(value) => context.addTextNode(value)
          case Fragment(xs) => xs.foreach(expand)
          case Empty => // do nothing
          case _ => // do nothing
        }
        Template {
          context.openNode(toKebabCase(name))
          children.foreach(expand)
          context.closeNode()
        }
      }
    }
    /** Tag's attribute. Like `<div class="header"></div>` */
    case class Attr(name: String, value: String) extends Document
    /** Text inside tag. Like `<h1>Hello world</h1>`*/
    case class Text(value: String) extends Document
    /** Nodes which are have to be added as a children of parent node.  */
    case class Fragment(xs: Seq[NodeLike]) extends NodeLike
    /** A way to add something specific to template. Events, delays, whatever. */
    case class Misc(value: MiscType) extends Document
    /** Just emptiness. */
    case object Empty extends NodeLike

    /** Programmatic template. */
    class Template(f: => Unit) extends NodeLike {
      def run(): Template = { f; this }
    }
    object Template {
      def apply[T](f: => Unit): Template =
        new Template(f)
    }
  }


  trait RenderContext {
    def openNode(name: String): Unit
    def closeNode(): Document.Node
    def setAttr(name: String, value: String): Document.Attr
    def addTextNode(text: String): Document.Text
    def addMisc(misc: Document.Misc): Unit
  }

  object RenderContext {
    /** That RenderContext does nothing */
    object Dummy extends RenderContext {
      val FakeNode = Document.Node("fake", Nil)
      val FakeText = Document.Text("fake")
      val FakeAttr = Document.Attr("fake", "fake")
      def openNode(name: String): Unit = ()
      def closeNode(): Document.Node = FakeNode
      def setAttr(name: String, value: String): Document.Attr = FakeAttr
      def addTextNode(text: String): Document.Text = FakeText
      def addMisc(misc: Document.Misc): Unit = ()
    }
  }

  val toKebabCase = ToKebabCase[String].convert _
}
