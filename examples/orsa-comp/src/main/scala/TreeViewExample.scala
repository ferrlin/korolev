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
    * @param items * @return
    */
  private def generateLI(items: Vector[ChildView], doGenerate: Boolean = true, ref: String): Vector[VDom.Node] =
    if (doGenerate)
      items map {
        case child@ChildView(_, item: LeafItem, parent) => 'li ('class /= "list-group-item",
          createCheckbox(item, ref)(childCheckboxEvent(child) _), item.text)
        case child@ChildView(isOpened, item: TreeItem, parent) => {
          createTree(item, Some(child))(None,
            createCheckbox(item, ref)(childCheckboxEvent(parent.get) _)
            , {
              generateLI(item.items, isOpened, ref)
            }
          )
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
        case c@ChildView(s, l: LeafItem, _) => c.copy(item = l.copy(checked = updatedStatus))
        case c@ChildView(s, t: TreeItem, _) =>
          val uT = t.copy(checked = updatedStatus, items = updateSubElements(t.items, updatedStatus))
          c.copy(item = uT)
      }
      val updatedTree = ref.copy(checked = updatedStatus, items = updatedChildren)
      val updatedEl = s.els + (item -> generateLI(updatedTree.items, ref = ref.text))
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
  def childCheckboxEvent(ref: View)(name: String, isChecked: Boolean): StateManager.Transition[State] = {
    case s: State => {

      val (_, selected) = ref match {
        case tv: TreeView => (tv.isOpened, tv.tree.text)
        case cv: ChildView => {
          println(s"GENEALOGY ${cv.genealogy}")
          cv.item match {
            case ti: TreeItem => (cv.isOpened, ti.text)
            case ci: LeafItem => (cv.isOpened, ci.text)
          }
        }
      }

      val isRoot = s.items.keySet.contains(selected)
      if (isRoot) {
        val TreeView(opened, parentRef) = s.items(selected)
        val updated: Vector[ChildView] = parentRef.items.map {
          case c@ChildView(_, l: LeafItem, _) if (l.text == name) =>
            c.copy(item = l.copy(checked = !isChecked))
          case c@ChildView(_, t: TreeItem, _) if (t.text == name) =>
            val newT = t.copy(checked = !isChecked, items = updateSubElements(t.items, !isChecked))
            c.copy(item = newT)
          case c@ChildView(_, i: State.Item, _) => c.copy(item = i)
        }

        val updatedRef = parentRef.copy(items = updated)
        val updatedEl = s.els + (selected -> generateLI(updatedRef.items, ref = selected))
        s.copy(items = s.items + (selected -> TreeView(opened, updatedRef)), els = updatedEl)
      } else {
        // when an item clicked is nested deeper
        val childRef = ref.asInstanceOf[ChildView]
        val selected = childRef.genealogy(0)

        println(s" GENEALOGY of ${childRef} checkbox#method ${childRef.genealogy}")

        val updatedRef = childRef.copy(item = childRef.item match {
          case l: LeafItem => l.copy(checked = !l.checked)
          case t: TreeItem => t.copy(checked = !t.checked)
        })

        val updatedView = traverse(updatedRef, s)
        println(s"Checkbox UPDATED view ${updatedView}")

        val TreeView(opened, parentRef) = s.items(selected)
        val updatedParentRef = parentRef.copy(items = updatedView)
        val updatedEls = s.els + (selected -> generateLI(updatedParentRef.items, ref = selected))
        s.copy(items = s.items + (selected -> TreeView(opened, updatedParentRef)), els = updatedEls)
        //        s
      }
    }
  }

  /**
    *
    * @return
    */
  def mainTreeEvent(target: TreeItem): StateManager.Transition[State] = {
    case s =>
      val selected = target.text
      val TreeView(isOpened, els) = s.items(selected)

      // reset other 'opened' state to false
      val other = s.items.filter(_._1 != selected).map(p => (p._1, p._2.copy(isOpened = false)))
      val updatedItems = (s.items ++ other) + (selected -> TreeView(!isOpened, els))

      val updatedEl = s.els + (selected -> generateLI(updatedItems(selected).tree.items, ref = selected))
      s.copy(selected = selected, items = updatedItems, els = updatedEl)
  }

  /**
    *
    * @return
    */
  def nestedTreeEvent(target: Option[ChildView]): StateManager.Transition[State] = {
    case s =>
      println(s"TARGET $target is tree view collapsed${target.get.isOpened}")

      val cv = target.get
      val toggle = !cv.isOpened
      val updated = cv.copy(isOpened = toggle)

      println(s"UPDATED NESTED TREE even $updated")
      val updatedItem = traverse(updated, s)

      println(s"UPDATE view ${updatedItem}")

      println("-----------------------------")
      println(s"OLD state $s")

      val selected = cv.genealogy(0)
      val temp: TreeView = s.items(selected)
      val updatedTree: TreeView = temp.copy(tree = s.items(selected).tree.copy(items = updatedItem))
      val updatedState = s.items + (selected -> updatedTree)

      println("-----------------------------")
      println(s"UPDATED state $updatedState")
      val updatedEl = s.els + (selected -> generateLI(updatedState(selected).tree.items, ref = selected))
      s.copy(items = updatedState, els = updatedEl)
  }

  /**
    * Creates the tree item with its event handling feature.
    *
    * @param item
    * @param state
    * @return
    */
  def createTree(item: TreeItem, view: Option[ChildView] = None)(state: Option[State], checkbox: VDom.Node, childrenEls: Vector[VDom.Node]): VDom.Node = {
    val (isSelected, isOpened, isFound) = if (state.isDefined)
      (item.text == state.get.selected, state.get.items(item.text).isOpened, state.get.items.contains(item.text))
    else if (view.isDefined) (true, view.get.isOpened, false)
    else (false, false, false)

    'li ('class /= "list-group-item",
      getStyleClass(isSelected && isOpened),
      checkbox,
      'span (
        event('click) {
          immediateTransition {
            if (isFound) mainTreeEvent(item)
            else {
              nestedTreeEvent(view)
            }
          }
        },
        getStyleFor(item.text, isSelected)
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
                createTree(state.items(item).tree)(Some(state), {
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


sealed trait View2

object View2 {

  case class TreeView(isOpened: Boolean, node: State.TreeItem) extends View2

}

case class State(selected: String = State.utv01.tree.text,
                 items: Map[String, TreeView] = Map(
                   State.utv01.tree.text -> State.utv01,
                   State.utv02.tree.text -> State.utv02,
                   State.utv03.tree.text -> State.utv03),
                 els: Map[String, Vector[VDom.Node]] = Map())

object View {

  import State._

  sealed trait View

  case class TreeView(isOpened: Boolean, tree: TreeItem) extends View

  /**
    * isOpened is defaulted to false for non
    * tree child view (ie Leaf).
    *
    */
  case class ChildView(isOpened: Boolean = true, item: Item, parent: Option[View]) extends View {


    lazy val genealogy: Seq[String] = getAncestry(parent)(Seq.empty[String]) :+ getText(item)
    lazy val depth = genealogy.length


    private def getText(item: Item): String = item match {
      case i: TreeItem => i.text
      case l: LeafItem => l.text
    }

    /**
      * Get the ancestry of this child view.
      */
    private def getAncestry(parent: Option[View])(seq: Seq[String]): Seq[String] = if (parent.isDefined) {
      parent.get match {
        case c: ChildView =>
          val name: String = getText(c.item)
          getAncestry(c.parent)(seq :+ name)
        case t: TreeView => t.tree.text +: seq
      }
    } else Nil
  }

  /**
    *
    * @param cv
    * @param state
    * @return
    */
  def traverse(cv: ChildView, state: State): Vector[ChildView] = {
    def iter(names: Seq[String], vs: Vector[ChildView]): Tuple2[ChildView, Vector[ChildView]] = {

      val res: (String, Vector[ChildView]) = vs.map {
        case c: ChildView => c.item match {
          case l: LeafItem => (l.text, Vector.empty[ChildView])
          case t: TreeItem => (t.text, t.items)
        }
      }.filter { case (n, _) => n == names.head }.head

      println("--------------------------------")
      println(s"RES value $res")
      println("--------------------------------")
      println(s"BEFORE VS values $vs")
      println("--------------------------------")

      val result: Vector[ChildView] = vs.map {
        case c: ChildView =>
          c.item match {
            case l: LeafItem if (l.text == names.head) =>
              if (names.tail.nonEmpty) iter(names.tail, res._2)._1 else cv
            case t: TreeItem if (t.text == names.head) =>
              if (names.tail.nonEmpty) iter(names.tail, res._2)._1
              else cv
            case _ => c
          }
      }

      println("--------------------------------")
      println(s"AFTER VS [result] values $result")
      println("--------------------------------")

      (result.head, result)
    }

    // index 0 refers to root parent..
    val (root, remains) = cv.genealogy match {
      case h :: tail => (h, tail)
    }

    println(s"GENEALOGY ${cv.genealogy}")

    iter(remains, state.items(root).tree.items)._2
  }
}

object State {
  val effects = Effects[Future, State, Any]

  sealed trait Item

  case class TreeItem(text: String, checked: Boolean = false, items: Vector[ChildView] = Vector.empty) extends Item

  case class LeafItem(text: String, checked: Boolean = false) extends Item

  object Item {
    def apply(n: Int, tv: Option[View] = None): Vector[ChildView] = (0 to n).toVector.map {
      l => {
        if (l < 2) {
          val nestedItem = TreeItem(s"Nested Tree #$l", checked = false)
          val nestedTree = ChildView(item = nestedItem, parent = tv)
          val childrenViews = (0 to 2).toVector.map { i =>
            ChildView(item = LeafItem(s"Nested Item $i", checked = false), parent = Some(nestedTree))
          }

          nestedTree.copy(item = nestedItem.copy(items = childrenViews))
        } else
          ChildView(item = LeafItem(s"Item #$l", checked = false), parent = tv)
      }
    }
  }

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

  val children01 = Item(3, Some(tv01))
  val children02 = Item(7, Some(tv02))
  val children03 = Item(10, Some(tv03))

  // added children to tree items
  val utv01 = tv01.copy(tree = ti01.copy(items = children01))
  val utv02 = tv02.copy(tree = ti02.copy(items = children02))
  val utv03 = tv03.copy(tree = ti03.copy(items = children03))

  // test..
  utv02.tree.items.foreach { cv =>
    cv.item match {
      case t: TreeItem => t.items.foreach(i => println(s">>> ${i.genealogy} with depth[${i.depth}]"))
      case _ => println(s">>> ${cv.genealogy} with depth[${cv.depth}]")
    }
  }
}