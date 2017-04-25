import korolev._
import korolev.server._
import korolev.blazeServer._
import korolev.execution._

import scala.concurrent.Future

object TreeView extends KorolevBlazeServer {

  import State.effects._

  val TREE_STYLE_PLUS = "icon expand-icon glyphicon glyphicon-plus"
  val TREE_STYLE_MINUS = "icon expand-icon glyphicon glyphicon-minus"

  val STYLE_CHECKED = "icon check-icon glyphicon glyphicon-cloud"
  val STYLE_UNCHECKED = "icon check-icon glyphicon glyphicon-unchecked"


  // Handler to input
  val inputId = elementId
  val storage = StateStorage.default[Future, State](State())

  /**
    * Tree element style when clicked/unclicked (ie '-' or '+')
    *
    * @param isSelected
    * @return
    */
  private def getStyleClass(isSelected: Boolean): VDom.Node =
    'span ('class /= (if (isSelected) TREE_STYLE_MINUS else TREE_STYLE_PLUS))

  private def createCheckbox(isChecked: Boolean, items: Map[String, Tuple2[Boolean, State.Tree]], item: String): VDom.Node = {
    'span ('class /= (if (items(item)._2.checked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click) {
        immediateTransition { case s =>
          val (opened, ref) = s.items(item)
          val updatedStatus = !ref.checked
          val updatedChildren = ref.items.map {
            case l: State.Leaf => l.copy(checked = updatedStatus)
            case t: State.Tree => t.copy(checked = updatedStatus)
          }
          val updatedTree = ref.copy(checked = updatedStatus, items = updatedChildren)
          val updatedEl = s.els + (item -> generateLI(updatedTree.items, ref.text))
          s.copy(items = s.items + (item -> (opened, updatedTree)), els = updatedEl)
        }
      }
    )
  }

  /**
    * Checkboxes used in children elements
    *
    * @param item
    * @return
    */
  private def createCheckbox(item: State.Item, parent: String): VDom.Node = {
    val (isChecked, name) = item match {
      case l: State.Leaf => (l.checked, l.text)
      case t: State.Tree => (t.checked, t.text)
    }
    'span ('class /= (if (isChecked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click) {
        immediateTransition { case s =>
          val (opened, parentRef) = s.items(parent)

          val updated: Vector[State.Item] = parentRef.items.map {
            case l: State.Leaf if (l.text == name) =>
              l.copy(checked = !isChecked)
            case t: State.Tree if (t.text == name) =>
              t.copy(checked = !isChecked)
            case i: State.Item => i
          }

          val updatedRef = parentRef.copy(items = updated)
          val updatedEl = s.els + (parent -> generateLI(updatedRef.items, parent))
          s.copy(items = s.items + (parent -> (opened, updatedRef)), els = updatedEl)
        }
      }
    )
  }


  /**
    * Style of the tree element name when clicked/unclicked
    *
    * @param isSelected
    * @return
    */
  private def getStyleFor(name: String, isSelected: Boolean): VDom.Node = {
    if (isSelected) 'strong (name) else 'i (name)
  }

  private def getChildrenEls(isSelected: Boolean)(els: Vector[VDom.Node]): Vector[VDom.Node] =
    if (isSelected && els.nonEmpty) els else Vector('br ())

  /**
    *
    * @param items
    * @return
    */
  private def generateLI(items: Vector[State.Item], ref: String): Vector[VDom.Node] = items map {
    case (item: State.Leaf) => 'li ('class /= "list-group-item",
      createCheckbox(item, ref), item.text)
    case (item: State.Tree) => 'li ('class /= "list-group-item",
      createCheckbox(item, ref), item.text
    )
  }

  val service = blazeService[Future, State, Any] from KorolevServiceConfig[Future, State, Any](
    stateStorage = storage,
    head = 'head (
      'title ("Simple Treeview Page"),
      'link (
        'href /= "/bootstrap.min.css",
        'rel /= "stylesheet",
        'type /= "text/css"),
      'link (
        'href /= "/treeview.css",
        'rel /= "stylesheet",
        'type /= "text/css")),
    render = {
      case state =>
        'body (
          'div ('class /= "treeview",
            'ul ('class /= "list-group",
              state.items.keys map { item =>
                'li ('class /= "list-group-item",
                  getStyleClass(item == state.selected && state.items(item)._1),
                  createCheckbox(state.items(item)._2.checked, state.items, item),
                  'span (
                    event('click) {
                      immediateTransition { case s =>
                        val (isOpened, els) = s.items(item)

                        // reset other 'opened' state to false
                        val other = s.items.filter(_._1 != item).map(p => (p._1, p._2.copy(_1 = false)))
                        val updatedItems = (s.items ++ other) + (item -> (!isOpened, els))

                        val updatedEl = s.els + (item -> generateLI(updatedItems(item)._2.items, item))
                        s.copy(selected = item, items = updatedItems, els = updatedEl)
                      }
                    },
                    getStyleFor(item, item == state.selected)
                  ), {
                    // check if the  selected item is used as a key for els in State
                    val elements = if (state.els.filterKeys(_ == state.selected).isEmpty) Vector() else state.els(state.selected)
                    getChildrenEls(item == state.selected && state.items(item)._1)(elements)
                  }
                )
              }
            )
          )
        )
    },
    serverRouter = {
      ServerRouter(
        dynamic = (_, _) => Router(
          fromState = {
            case State(tab, _, _) =>
              Root / tab
          },
          toState = {
            case (s, Root) =>
              val u = s.copy(selected = s.items.keys.head)
              Future.successful(u)
            case (s, Root / name) =>
              val key = s.items.keys.find(_.toLowerCase() == name)
              Future.successful(key.fold(s)(k => s.copy(selected = k)))
          }
        ),
        static = (deviceId) => Router(
          toState = {
            case (_, Root) =>
              storage.initial(deviceId)
            case (_, Root / name) =>
              storage.initial(deviceId) map { s =>
                val key = s.items.keys.find(_.toLowerCase == name)
                key.fold(s)(k => s.copy(selected = k))
              }
          }
        )
      )
    }
  )
}

case class State(selected: String = State.default.text,
                 items: Map[String, Tuple2[Boolean, State.Tree]] = Map(
                   State.default.text -> (false, State.default),
                   State.second.text -> (false, State.second),
                   State.third.text -> (false, State.third)),
                 els: Map[String, Vector[VDom.Node]] = Map())

object State {
  val effects = Effects[Future, State, Any]

  sealed trait Item

  case class Tree(text: String, checked: Boolean = false, items: Vector[State.Item]) extends Item

  case class Leaf(text: String, checked: Boolean = false) extends Item

  object Item {
    def apply(n: Int): Vector[Leaf] = (0 to n).toVector.map {
      i => Leaf(s"Item #$i", checked = false)
    }
  }

  // sample test instances
  val default = Tree("Tree1", false, Item(5))
  val second = Tree("Tree2", false, Item(7))
  val third = Tree("Tree3", false, Item(2))

}