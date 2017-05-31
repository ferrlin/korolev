
import Item.TreeItem
import View.TreeView
import korolev._
import korolev.blazeServer._
import korolev.execution._
import korolev.server._
import org.http4s.blaze.http

import scala.concurrent.Future

object TreeViewExample extends KorolevBlazeServer {

  import State.effects._

  // Handler to input
  val inputId: Effects.ElementId = elementId
  val storage: StateStorage[Future, State] = StateStorage.default[Future, State](State(Init.default))
  val service: http.HttpService = blazeService[Future, State, Any] from KorolevServiceConfig[Future, State, Any](
    stateStorage = storage,
    head = 'head (
      'title ("Simple Treeview Page"),
      'link (
        'href /= "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css",
        'rel /= "stylesheet",
        'type /= "text/css"),
      'link (
        'href /= "/treeview.css",
        'rel /= "stylesheet",
        'type /= "text/css")),
    render = {
      case state =>
        'body (
          // Generates the Virtual Node DOM for tree component with
          // state as the input
          TreeViewComponent(state)
        )
    }
    ,
    serverRouter = {
      ServerRouter(
        dynamic = (_, _) => Router(
          fromState = {
            case State(tree) =>
              Root / tree.rootSelected
          },
          toState = {
            case (s, Root) => {
              val u = s.copy(treeViewState = s.treeViewState.copy(rootSelected = s.treeViewState.items.keys.head))
              Future.successful(u)
            }
            case (s, Root / name) => {
              val key = s.treeViewState.items.keys.find(_.toLowerCase() == name)
              val result = if (key.isDefined)
                s.copy(treeViewState = s.treeViewState.copy(rootSelected = key.get))
              else s

              //key.fold(s)(k => s.copy(rootSelected = k))

              Future.successful(result)
            }
          }
        ),
        static = (deviceId) => Router(
          toState = {
            case (_, Root) =>
              storage.initial(deviceId)
            case (_, Root / name) =>
              storage.initial(deviceId) map { s => {
                val key = s.treeViewState.items.keys.find(_.toLowerCase() == name)
                val result = if (key.isDefined)
                  s.copy(treeViewState = s.treeViewState.copy(rootSelected = key.get))
                else s

                //key.fold(s)(k => s.copy(rootSelected = k))

               result
              }
              }
          }
        )
      )
    }
  )
}

object Init {


  // sample test instances
  val defaultName = "Tree1"
  val secondName = "Tree2"
  val thirdName = "Tree3"

  val ti01: TreeItem = TreeItem(defaultName, checked = false, Vector.empty)
  val ti02: TreeItem = TreeItem(secondName, checked = false, Vector.empty)
  val ti03: TreeItem = TreeItem(thirdName, checked = false, Vector.empty)

  val tv01: TreeView = TreeView(false, ti01)
  val tv02: TreeView = TreeView(false, ti02)
  val tv03: TreeView = TreeView(false, ti03)
  //
  val children01 = Item(3, Some(tv01))
  val children02 = Item(7, Some(tv02))
  val children03 = Item(10, Some(tv03))

  /*
    val d01 = childTree(1, Some(tv01))
    val d02 = childTree(2, Some(d01))
    val d03 = childTree(3, Some(d02))
    val d04 = childTree(4, Some(d03))

    val d05 = childLeaf(5, Some(d04))

    val ud04 = d04.copy(item = d04.item match {
      case t: TreeItem => t.copy(items = Vector(d05))
      case i: Item => i
    })

    val ud03 = d03.copy(item = d03.item match {
      case t: TreeItem => t.copy(items = Vector(ud04))
      case i: Item => i
    })

    val ud02 = d02.copy(item = d02.item match {
      case t: TreeItem => t.copy(items = Vector(ud03))
      case i: Item => i
    })

    val ud01 = d01.copy(item = d01.item match {
      case t: TreeItem => t.copy(items = Vector(ud02))
      case i: Item => i
    })

    val fd04 = ud04.copy(parent = Some(ud03))
    val fd03 = ud03.copy(parent = Some(ud02))
    val fd02 = ud02.copy(parent = Some(ud01))

    val fd01 = ud01.copy(item = ud01.item match {
      case t: TreeItem => t.copy(items = Vector(fd02))
      case i: Item => i
    })
  */
  // added children to tree items
  val utv01: TreeView = tv01.copy(tree = ti01.copy(items = children01 /*:+ fd01*/))
  val utv02: TreeView = tv02.copy(tree = ti02.copy(items = children02))
  val utv03: TreeView = tv03.copy(tree = ti03.copy(items = children03))

  // The default state for the tree view component.
  val default =
    TreeViewState(utv01.tree.text,
      itemSelected = None,
      items = Map(
        utv01.tree.text -> utv01,
        utv02.tree.text -> utv02,
        utv03.tree.text -> utv03),
      els = Map.empty)
}