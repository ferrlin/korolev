import korolev._
import korolev.server._
import korolev.blazeServer._
import korolev.execution._

import scala.concurrent.Future

object CollapsibleMenu extends KorolevBlazeServer {

  import State.effects._

  val MENU_STYLE_CLOUD = "glyphicon glyphicon-cloud"

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
    'span ('class /= MENU_STYLE_CLOUD)

  /**
    * Style of the tree element name when clicked/unclicked
    *
    * @param item
    * @param isSelected
    * @return
    */
  private def getStyleFor(item: State.Item, isSelected: Boolean): VDom.Node = {
    val name = item match {
      case h: State.Head => h.name
      case s: State.Sub => s.name
    }
    if (isSelected) 'strong (name) else 'i (name)
  }

  private def getChildrenEls(isSelected: Boolean)(els: Vector[VDom.Node]): Vector[VDom.Node] =
    if (isSelected) els else Vector('br ())

  //    if (isSelected) 'div ('class /= "collapse",
  //      'ul ('class /= "nav nav-list", els)) else 'br ()

  /**
    *
    * @param items
    * @return
    */
  private def generateLI(items: Vector[State.Item]): Vector[VDom.Node] = items map {
    case (item: State.Sub) =>
      'li ('class /= "list-group-item",
        item.name)
    case (item: State.Head) => 'li ('class /= "list-group-item",
      event('click) {
        immediateTransition { case s => s }
      },
      item.name
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
        'href /= "/collapsible.css",
        'rel /= "stylesheet",
        'type /= "text/css")),
    render = {
      case state =>
        'body (
          'div ('class /= "navbar-collapse collapse sidebar-navbar-collapse",
            'ul ('class /= "nav navbar-nav",
              state.items.keys map { case item: State.Head =>
                'li (getStyleClass(item == state.selected),
                  'span (
                    event('click) {
                      immediateTransition { case s =>
                        val (isOpened, els) = s.items(item)

                        // reset other 'opened' state to false
                        val other = s.items.filter(_._1 != item).map(p => (p._1, p._2.copy(_1 = false)))
                        val updatedItems = (s.items ++ other) + (item -> (!isOpened, els))

                        s.copy(selected = item, items = updatedItems, els = generateLI(updatedItems(item)._2))
                      }
                    },
                    getStyleFor(item, item == state.selected)
                  ),
                  getChildrenEls(item == state.selected && state.items(item)._1)(state.els)
                )
              case _ => 'br ()
              }
            )
          )
        )
    },
    serverRouter = {
      ServerRouter(
        dynamic = (_, _) => Router(
          fromState = {
            case State(tab: State.Head, _, _) =>
              Root / tab.name
            case State(tab: State.Sub, _, _) =>
              Root / tab.name
          },
          toState = {
            case (s, Root) =>
              val u = s.copy(selected = s.items.keys.head)
              Future.successful(u)
            case (s, Root / name) =>
              val key = s.items.keys.find {
                case h: State.Head => h.name.toLowerCase() == name
                case s: State.Sub => s.name.toLowerCase() == name
              }
              Future.successful(key.fold(s) {
                case k: State.Head => s.copy(selected = k)
                case _ => s
              })
          }
        ),
        static = (deviceId) => Router(
          toState = {
            case (_, Root) =>
              storage.initial(deviceId)
            case (_, Root / name) =>
              storage.initial(deviceId) map { s =>
                val key = s.items.keys.find {
                  case h: State.Head => h.name.toLowerCase() == name
                  case s: State.Sub => s.name.toLowerCase() == name
                }
                key.fold(s) { k => s.copy(selected = k) }
              }
          }
        )
      )
    }
  )
}

case class State(selected: State.Item = State.default,
                 items: Map[State.Item, Tuple2[Boolean, Vector[State.Item]]] = Map(
                   State.default -> (false, State.default.items),
                   State.second -> (false, State.second.items),
                   State.third -> (false, State.third.items)),
                 els: Vector[VDom.Node] = Vector())

object State {
  val effects = Effects[Future, State, Any]

  sealed trait Item

  case class Head(name: String, items: Vector[State.Sub]) extends Item

  case class Sub(name: String) extends Item

  object Item {
    def apply(n: Int): Vector[State.Sub] = (0 to n).toVector.map {
      i => Sub(s"Menu Item #$i")
    }
  }

  // test samples
  val default = Head("Menu 1", Item(3))
  val second = Head("Menu 2", Item(7))
  val third = Head("Menu 3", Item(2))
}