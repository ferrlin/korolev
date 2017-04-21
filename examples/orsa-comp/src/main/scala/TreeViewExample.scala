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

  private def createCheckbox(isChecked: Boolean, selected: String): VDom.Node = {
    'span ('class /= (if (isChecked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click) {
        immediateTransition { case s =>
          println(s"ISCHECKED ::: $isChecked")
          val (opened, ref) = s.items(selected)
          val updatedTree = ref.copy(checked = !isChecked)
          s.copy(items = s.items + (selected -> (opened, updatedTree)))
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
    if (isSelected) (els) else Vector('br ())

  /**
    *
    * @param items
    * @return
    */
  private def generateLI(items: Vector[State.Item]): Vector[VDom.Node] = items map {
    case (item: State.Leaf) => 'li ('class /= "list-group-item",
      'div (
        'class /= {
          if (!item.checked) "checkbox" else "checkbox checkbox_checked"
        }
      ), item.text)
    case (item: State.Tree) => 'li ('class /= "list-group-item",
      'div (
        'class /= {
          if (!item.checked) "checkbox" else "checkbox checkbox_checked"
        }
      ),
      event('click) {
        immediateTransition { case s =>

          s
        }
      },
      item.text
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
                  'span ('class /= (if (item == state.selected && state.items(state.selected)._2.checked) STYLE_CHECKED else STYLE_UNCHECKED),
                    event('click) {
                      immediateTransition { case s =>
                        println(s"FALSE or TRUE ????${(state.items(s.selected)._2.checked)} ::: selected ${s.selected}")
                        val (opened, ref) = s.items(item)
                        val updatedTree = ref.copy(checked = !ref.checked)
                        println(s"Updated tree ::: ${updatedTree}")
                        s.copy(items = s.items + (item -> (opened, updatedTree)))
                      }
                    }
                  ),
                  'span (
                    event('click) {
                      immediateTransition { case s =>
                        val (isOpened, els) = s.items(item)

                        // reset other 'opened' state to false
                        val other = s.items.filter(_._1 != item).map(p => (p._1, p._2.copy(_1 = false)))
                        val updatedItems = (s.items ++ other) + (item -> (!isOpened, els))

                        s.copy(selected = item, items = updatedItems, els = generateLI(updatedItems(item)._2.items))
                      }
                    },
                    getStyleFor(item, item == state.selected)
                  ),
                  getChildrenEls(item == state.selected && state.items(item)._1)(state.els)
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
                 els: Vector[VDom.Node] = Vector())

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