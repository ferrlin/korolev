import korolev._
import korolev.server._
import korolev.blazeServer._
import korolev.execution._

import scala.concurrent.Future
import State._
import View._

object TreeViewExample extends KorolevBlazeServer {

  import State.effects._

  val TREE_STYLE_PLUS = "icon expand-icon glyphicon glyphicon-plus"
  val TREE_STYLE_MINUS = "icon expand-icon glyphicon glyphicon-minus"

  val STYLE_CHECKED = "icon check-icon glyphicon glyphicon-cloud"
  val STYLE_UNCHECKED = "icon check-icon glyphicon glyphicon-unchecked"


  // Handler to input
  val inputId: Effects.ElementId = elementId
  val storage: StateStorage[Future, State] = StateStorage.default[Future, State](State())

  /**
    * Tree element style when clicked/unclicked (ie '-' or '+')
    *
    * @param isSelected
    * @return
    */
  private def getStyleClass(isSelected: Boolean): VDom.Node =
    'span ('class /= (if (isSelected) TREE_STYLE_MINUS else TREE_STYLE_PLUS))

  /**
    *
    * @param items
    * @param item
    * @param t
    * @return
    */
  private def createCheckbox(items: Map[String, TreeView], item: String)(t: (String) => StateManager.Transition[State]): VDom.Node = {
    'span ('class /= (if (items(item).tree.checked) STYLE_CHECKED else STYLE_UNCHECKED),
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
      case l: LeafItem => (l.checked, l.text)
      case t: TreeItem => (t.checked, t.text)
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
  private def generateLI(items: Vector[ChildView], ref: String, doGenerate: Boolean = true): Vector[VDom.Node] =
    if (doGenerate)
      items map {
        case ChildView(isOpened, item: LeafItem) => 'li ('class /= "list-group-item",
          createCheckbox(item, ref)(childCheckboxEvent(ref) _), item.text)
        case ChildView(isOpened, item: TreeItem) => {
          createTree(item.text)(None, {
            createCheckbox(item, ref)(childCheckboxEvent(ref) _)
          }, {
            generateLI(item.items, item.text, isOpened)
          })
        }
      }
    else Vector()


  /**
    * Event used for parent tree when checkbox is clicked
    *
    * @param item
    * @return
    */
  def parentCheckboxEvent(item: String): StateManager.Transition[State] = {
    case s =>
      val TreeView(opened, ref) = s.items(item)
      val updatedStatus = !ref.checked
      val updatedChildren = ref.items.map {
        case c@ChildView(s, l: LeafItem) => c.copy(item = l.copy(checked = updatedStatus))
        case c@ChildView(s, t: TreeItem) =>
          val uT = t.copy(checked = updatedStatus, items = updateSubElements(t.items, updatedStatus))
          c.copy(item = uT)
      }
      val updatedTree = ref.copy(checked = updatedStatus, items = updatedChildren)
      val updatedEl = s.els + (item -> generateLI(updatedTree.items, ref.text))
      s.copy(items = s.items + (item -> TreeView(opened, updatedTree)), els = updatedEl)
  }

  /**
    * Recursively updates the checkbox value.
    *
    * @param children
    * @param status
    * @return
    */
  private def updateSubElements(children: Vector[ChildView], status: Boolean): Vector[ChildView] =
    children.map {
      case c => c.copy(item = c.item match {
        case t: TreeItem => t.copy(checked = status, items = updateSubElements(t.items, status))
        case l: LeafItem => l.copy(checked = status)
      })
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
      val TreeView(opened, parentRef) = s.items(ref)
      val updated: Vector[ChildView] = parentRef.items.map {
        case c@ChildView(_, l: LeafItem) if (l.text == name) =>
          c.copy(item = l.copy(checked = !isChecked))
        case c@ChildView(_, t: TreeItem) if (t.text == name) =>
          val newT = t.copy(checked = !isChecked, items = updateSubElements(t.items, isChecked))
          c.copy(item = newT)
        case c@ChildView(_, i: State.Item) => c.copy(item = i)
      }

      val updatedRef = parentRef.copy(items = updated)
      val updatedEl = s.els + (ref -> generateLI(updatedRef.items, ref))
      s.copy(items = s.items + (ref -> TreeView(opened, updatedRef)), els = updatedEl)
  }

  /**
    *
    * @param selected
    * @return
    */
  def mainTreeEvent(selected: String): StateManager.Transition[State] = {
    case s =>
      val TreeView(isOpened, els) = s.items(selected)

      // reset other 'opened' state to false
      val other = s.items.filter(_._1 != selected).map(p => (p._1, p._2.copy(isOpened = false)))
      val updatedItems = (s.items ++ other) + (selected -> TreeView(!isOpened, els))

      val updatedEl = s.els + (selected -> generateLI(updatedItems(selected).tree.items, selected))
      s.copy(selected = selected, items = updatedItems, els = updatedEl)
  }

  /**
    *
    * @param selected
    * @return
    */
  def nestedTreeEvent(parentSelected: String, selected: String*): StateManager.Transition[State] = {
    case s =>
      s
  }

  /**
    * Creates the tree item with its event handling feature.
    *
    * @param item
    * @param state
    * @return
    */
  def createTree(item: String)(state: Option[State], checkbox: VDom.Node, childrenEls: Vector[VDom.Node]): VDom.Node = {
    val (isSelected, isOpened, isFound) = if (state.isDefined)
      (item == state.get.selected, state.get.items(item).isOpened, state.get.items.contains(item))
    else
      (false, false, false)

    'li ('class /= "list-group-item",
      getStyleClass(isSelected && isOpened),
      checkbox,
      'span (
        event('click) {
          immediateTransition {
            if (isFound) mainTreeEvent(item) else nestedTreeEvent(item)
          }
        },
        getStyleFor(item, isSelected)
      ), childrenEls
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
                createTree(item)(Some(state), {
                  createCheckbox(state.items, item)(parentCheckboxEvent _)
                }, {
                  // check if the  selected item is used as a key for els in State
                  val elements = if (state.els.filterKeys(_ == state.selected).isEmpty) Vector() else state.els(state.selected)
                  getChildrenEls(item == state.selected && state.items(item).isOpened)(elements)
                })
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
                 items: Map[String, TreeView] = Map(
                   State.default.text -> TreeView(isOpened = false, tree = State.default),
                   State.second.text -> TreeView(isOpened = false, tree = State.second),
                   State.third.text -> TreeView(isOpened = false, tree = State.third)),
                 els: Map[String, Vector[VDom.Node]] = Map())

object View {

  import State._

  case class TreeView(isOpened: Boolean, tree: TreeItem)

  /**
    * isOpened is defaulted to false for non
    * tree child view (ie Leaf).
    *
    * @param isOpened
    * @param item
    */
  case class ChildView(isOpened: Boolean = false, item: Item)

}

object State {
  val effects = Effects[Future, State, Any]

  sealed trait Item

  case class TreeItem(text: String, checked: Boolean = false, items: Vector[ChildView]) extends Item

  case class LeafItem(text: String, checked: Boolean = false) extends Item

  object Item {
    def apply(n: Int): Vector[ChildView] = (0 to n).toVector.map {
      l => {
        if (l < 2) {
          val children = (0 to 2).toVector.map { i =>
            ChildView(item = LeafItem(s"Nested Item $i", checked = false))
          }
          ChildView(item = TreeItem(s"Nested Tree #$l", checked = false, children))
        } else
          ChildView(item = LeafItem(s"Item #$l", checked = false))
      }
    }
  }

  // sample test instances
  val default = TreeItem("Tree1", checked = false, Item(5))
  val second = TreeItem("Tree2", checked = false, Item(7))
  val third = TreeItem("Tree3", checked = false, Item(2))

}