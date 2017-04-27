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

  private def createCheckbox(items: Map[String, Tuple2[Boolean, State.Tree]], item: String)(t: (String) => StateManager.Transition[State]): VDom.Node = {
    'span ('class /= (if (items(item)._2.checked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click) {
        immediateTransition {
          t(item)
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
  private def createCheckbox(item: State.Item, parent: String)(t: (String, Boolean) => StateManager.Transition[State]): VDom.Node = {
    val (isChecked, name) = item match {
      case l: State.Leaf => (l.checked, l.text)
      case t: State.Tree => (t.checked, t.text)
    }
    'span ('class /= (if (isChecked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click) {
        immediateTransition {
          t(name, isChecked)
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
      createCheckbox(item, ref)(childCheckboxEvent(ref) _), item.text)
    case (item: State.Tree) => {
      'li ('class /= "list-group-item",
        createCheckbox(item, ref)(childCheckboxEvent(ref) _), item.text, generateLI(item.items, item.text))
    }
  }


  /**
    * Event used for parent tree when checkbox is clicked
    *
    * @param item
    * @return
    */
  def parentCheckboxEvent(item: String): StateManager.Transition[State] = {
    case s =>
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

  /**
    * Event for children (used in dealing with checkbox click
    *
    * @param ref
    * @param name
    * @param isChecked
    * @return
    */
  def childCheckboxEvent(ref: String)(name: String, isChecked: Boolean): StateManager.Transition[State] = {
    case s: State =>
      val (opened, parentRef) = s.items(ref)

      val updated: Vector[State.Item] = parentRef.items.map {
        case l: State.Leaf if (l.text == name) =>
          l.copy(checked = !isChecked)
        case t: State.Tree if (t.text == name) =>
          t.copy(checked = !isChecked)
        case i: State.Item => i
      }

      val updatedRef = parentRef.copy(items = updated)
      val updatedEl = s.els + (ref -> generateLI(updatedRef.items, ref))
      s.copy(items = s.items + (ref -> (opened, updatedRef)), els = updatedEl)
  }

  def mainTreeEvent(selected: String): StateManager.Transition[State] = {
    case s =>
      val (isOpened, els) = s.items(selected)

      // reset other 'opened' state to false
      val other = s.items.filter(_._1 != selected).map(p => (p._1, p._2.copy(_1 = false)))
      val updatedItems = (s.items ++ other) + (selected -> (!isOpened, els))

      val updatedEl = s.els + (selected -> generateLI(updatedItems(selected)._2.items, selected))
      s.copy(selected = selected, items = updatedItems, els = updatedEl)
  }

  /**
    * Creates the tree item with its event handling feature.
    *
    * @param item
    * @param state
    * @return
    */
  def createTree(item: String)(state: State): VDom.Node = 'li ('class /= "list-group-item",
    getStyleClass(item == state.selected && state.items(item)._1),
    createCheckbox(state.items, item)(parentCheckboxEvent _),
    'span (
      event('click) {
        immediateTransition {
          //if main tree
          mainTreeEvent(item)
          // else
          // nestedTreeEvent(item)
        }
      },
      getStyleFor(item, item == state.selected)
    ), {
      // check if the  selected item is used as a key for els in State
      val elements = if (state.els.filterKeys(_ == state.selected).isEmpty) Vector() else state.els(state.selected)
      getChildrenEls(item == state.selected && state.items(item)._1)(elements)
    }
  )

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
              state.items.keys map { item => createTree(item)(state) }
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
    def apply(n: Int): Vector[Item] = (0 to n).toVector.map {
      l => {
        if (l < 2) {
          val leafs = (0 to 2).toVector.map { i =>
            Leaf(s"Nested Item $i", checked = false)
          }
          Tree(s"Nested Tree #$l", checked = false, leafs)
        } else
          Leaf(s"Item #$l", checked = false)
      }
    }
  }

  // sample test instances
  val default = Tree("Tree1", false, Item(5))
  val second = Tree("Tree2", false, Item(7))
  val third = Tree("Tree3", false, Item(2))

}