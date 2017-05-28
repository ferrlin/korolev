import korolev._
import korolev.blazeServer._
import korolev.execution._
import korolev.server._

import scala.concurrent.Future

object TreeViewExample extends KorolevBlazeServer {

  import TreeViewState.effects._

  // Handler to input
  val inputId: Effects.ElementId = elementId
  val storage: StateStorage[Future, TreeViewState] = StateStorage.default[Future, TreeViewState](TreeViewState())


  val service = blazeService[Future, TreeViewState, Any] from KorolevServiceConfig[Future, TreeViewState, Any](
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
          //TODO: add Treeview component here
          /* 'div ('class /= "treeview",
             'ul ('class /= "list-group",
               state.items.keys.flatMap { item =>
                 // Create the root tree with/out child elements
                 createTree(Some(state.items(item).tree))(Some(state), {
                   createCheckbox(state.items, item)(parentCheckboxEvent _)
                 }, {
                   // check if the  selected item is used as a key for els in State
                   val elements =
                     if (state.els.filterKeys(_ == state.rootSelected).isEmpty) Vector.empty
                     else state.els(state.rootSelected)
                   getChildrenEls(/*item == state.selected &&*/ state.items(item).isOpened)(elements)
                 })
               }
             )
           )*/
        )
    }
    ,
    serverRouter = {
      ServerRouter(
        dynamic = (_, _) => Router(
          fromState = {
            case TreeViewState(tab, _, _, _) =>
              Root / tab
          },
          toState = {
            case (s, Root) =>
              val u = s.copy(rootSelected = s.items.keys.head)
              Future.successful(u)
            case (s, Root / name) =>
              val key = s.items.keys.find(_.toLowerCase() == name)
              Future.successful(key.fold(s)(k => s.copy(rootSelected = k)))
          }
        ),
        static = (deviceId) => Router(
          toState = {
            case (_, Root) =>
              storage.initial(deviceId)
            case (_, Root / name) =>
              storage.initial(deviceId) map { s =>
                val key = s.items.keys.find(_.toLowerCase == name)
                key.fold(s)(k => s.copy(rootSelected = k))
              }
          }
        )
      )
    }
  )

  // sample test instances
  //val defaultName = "Tree1"
  //val secondName = "Tree2"
  //val thirdName = "Tree3"

  //val ti01: TreeItem = TreeItem(defaultName, checked = false, Vector.empty)
  //val ti02: TreeItem = TreeItem(secondName, checked = false, Vector.empty)
  //val ti03: TreeItem = TreeItem(thirdName, checked = false, Vector.empty)

  //val tv01: TreeView = TreeView(false, ti01)
  //val tv02: TreeView = TreeView(false, ti02)
  //val tv03: TreeView = TreeView(false, ti03)

  //val children01 = Item(3, Some(tv01))
  //val children02 = Item(7, Some(tv02))
  //val children03 = Item(10, Some(tv03))

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
  //val utv01: TreeView = tv01.copy(tree = ti01.copy(items = children01 /*:+ fd01*/))
  //val utv02: TreeView = tv02.copy(tree = ti02.copy(items = children02))
  //val utv03: TreeView = tv03.copy(tree = ti03.copy(items = children03))

  // test..
  //  utv02.tree.items.foreach { cv =>
  //    cv.item match {
  //      case t: TreeItem => t.items.foreach(i => println(s">>> ${i.genealogy} with depth[${i.depth}]"))
  //      case _ => println(s">>> ${cv.genealogy} with depth[${cv.depth}]")
  //    }
  //  }
}