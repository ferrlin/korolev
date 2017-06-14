
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
  val storage: StateStorage[Future, State] = StateStorage.default[Future, State](Initialize(Init.default))
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
          // Generates the Virtual Node DOM for tree component with state as the input
          TreeViewComponent(state)
        )
    }, serverRouter = ServerRouter.empty
  )
}

object Init {

  // sample test instances
  val defaultName = "Tree1"
  val secondName = "Tree2"
  val thirdName = "Tree3"

  val ti01: TreeItem = TreeItem(defaultName, checked = false, Vector.empty, id = java.util.UUID.fromString("57eeb9b7-e693-3b71-8f81-bc275b37ccc4"))
  val ti02: TreeItem = TreeItem(secondName, checked = false, Vector.empty)
  val ti03: TreeItem = TreeItem(thirdName, checked = false, Vector.empty)


  val tv01: TreeView = TreeView(true, ti01)
  val tv02: TreeView = TreeView(true, ti02)
  val tv03: TreeView = TreeView(true, ti03)
  //
  val children01 = Item(3, Some(tv01), ids = Seq(java.util.UUID.fromString("04e67768-7958-4af5-bb1c-fbc358849709")))
  val children02 = Item(7, Some(tv02))
  val children03 = Item(10, Some(tv03))


  val altChildren02 = Item(1, Some(tv02))

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

  //  val uti01 = set"$ti01.$items" := children01

  //  val utv01 = set"$tv01.$tree" := ti01
  val utv01: TreeView = tv01.copy(tree = ti01.copy(items = children01 /*:+ fd01*/))
  val utv02: TreeView = tv02.copy(tree = ti02.copy(items = children02))
  val utv03: TreeView = tv03.copy(tree = ti03.copy(items = children03))

  val demoId = java.util.UUID.fromString("112a59be-79ff-3d91-9b84-d3bf6dca6939") //expects a compound id of
  // The default state for the tree view component.
  val default =
  TreeViewState(
    itemSelected = None,
    alt = Map(
      demoId -> Map(utv02.tree.text -> TreeView(isOpened = true,
        tree = ti02.copy(items = altChildren02)))), // for new feature
    items = Map(
      utv01.tree.text -> utv01,
      utv02.tree.text -> utv02,
      utv03.tree.text -> utv03),
    els = Map.empty)
}