import State._
import View._
import korolev._
import korolev.blazeServer._
import korolev.execution._
import korolev.server._

import scala.concurrent.Future

object TreeViewExample extends KorolevBlazeServer {

  import State.effects._

  val TREE_STYLE_PLUS = "icon expand-icon glyphicon glyphicon-plus"
  val TREE_STYLE_MINUS = "icon expand-icon glyphicon glyphicon-minus"

  val STYLE_CHECKED = "icon check-icon glyphicon glyphicon-check"
  val STYLE_UNCHECKED = "icon check-icon glyphicon glyphicon-unchecked"

  val STYLE_LIST_ITEM = "list-group-item node-treeview-checkable"
  val STYLE_TREE_ITEM = "list-group-item node-treeview-checkable"
  val STYLE_SUB_ITEM = ""


  // Handler to input
  val inputId: Effects.ElementId = elementId
  val storage: StateStorage[Future, State] = StateStorage.default[Future, State](State())

  /**
    * Tree element style when clicked/unclicked (ie '-' or '+')
    *
    * @param isSelected
    * @return
    */
  private def getTreeStyleClass(isSelected: Boolean, root: Option[TreeItem] = None, nested: Option[ChildView] = None)(t: (TreeItem) => StateManager.Transition[State])(n: (Option[ChildView]) => StateManager.Transition[State]): VDom.Node =
    'span ('class /= (if (isSelected) TREE_STYLE_MINUS else TREE_STYLE_PLUS),
      event('click) {
        immediateTransition {
          if (nested.isDefined) n(nested) else t(root.get)
        }
      }
    )

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
    if (isSelected) 'strong (name) else 'span (name)
  }

  private def getChildrenEls(isSelected: Boolean)(els: Vector[VDom.Node]): Vector[VDom.Node] =
    if (isSelected && els.nonEmpty) els else Vector.empty[VDom.Node]

  /**
    *
    * @param items * @return
    */
  private def generateLI(items: Vector[ChildView], doGenerate: Boolean = true, ref: String): Vector[VDom.Node] =
    if (doGenerate)
      items.flatMap {
        case child@ChildView(_, item: LeafItem, parent) => Seq(
          'li ('class /= STYLE_LIST_ITEM,
            createCheckbox(item, ref)(childCheckboxEvent(child) _), item.text))
        case child@ChildView(isOpened, item: TreeItem, parent) => {
          createTree(item, Some(child))(None,
            createCheckbox(item, ref)(childCheckboxEvent(parent.get) _)
            , {
              generateLI(item.items, isOpened, ref)
            })
        }
      }
    else Vector.empty[VDom.Node]


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
        val selected = childRef.genealogy.head
        val updatedRef = childRef.copy(item = childRef.item match {
          case l: LeafItem => l.copy(checked = !l.checked)
          case t: TreeItem => t.copy(checked = !t.checked)
        })

        val updatedItem = traverse(updatedRef, s)

        val TreeView(opened, parentRef) = s.items(selected)
        val updatedItems = updateRootValue(parentRef, updatedItem)
        val updatedParentRef = parentRef.copy(items = updatedItems)

        val updatedEls = s.els + (selected -> generateLI(updatedParentRef.items, ref = selected))
        s.copy(items = s.items + (selected -> TreeView(opened, updatedParentRef)), els = updatedEls)
      }
    }
  }

  private def updateRootValue(root: TreeItem, child: Option[ChildView]): Vector[ChildView] = {
    require(child.isDefined, "Child should be defined.")
    val childText = child.get.getText

    // updated the root (treeview) with the updated view
    root.items.map { old =>
      old.item match {
        case t: TreeItem if (t.text == childText) => child.get
        case l: LeafItem if (l.text == childText) => child.get
        case _ => old
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

      val cv = target.get
      val toggle = !cv.isOpened
      val updated = cv.copy(isOpened = toggle)
      val updatedItem = traverse(updated, s)

      val selected = cv.genealogy(0)
      val tree@TreeView(_, parentRef) = s.items(selected)

      val updatedItems = updateRootValue(parentRef, updatedItem)
      val updatedTree: TreeView = tree.copy(tree = s.items(selected).tree.copy(items = updatedItems))
      val updatedState = s.items + (selected -> updatedTree)
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
  def createTree(item: TreeItem, view: Option[ChildView] = None)(state: Option[State], checkbox: VDom.Node, childrenEls: Vector[VDom.Node]): Vector[VDom.Node] = {
    val (isSelected, isOpened, isFound) = if (state.isDefined)
      (item.text == state.get.selected, state.get.items(item.text).isOpened, state.get.items.contains(item.text))
    else if (view.isDefined) (true, view.get.isOpened, false)
    else (false, false, false)

    val items: Vector[VDom.Node] = for (
      el <- childrenEls
    ) yield 'li ('class /= STYLE_LIST_ITEM, el)

    Vector('li ('class /= STYLE_TREE_ITEM,
      getTreeStyleClass(isSelected && isOpened, Some(item), view)(mainTreeEvent _)(nestedTreeEvent _),
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
        getStyleFor(item.text, isSelected && isOpened)
      ), items)) // ++ items
  }

  val service = blazeService[Future, State, Any] from KorolevServiceConfig[Future, State, Any](
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
          'div ('class /= "treeview",
            'ul ('class /= "list-group",
              state.items.keys.flatMap { item =>

                /** start **/
                createTree(state.items(item).tree)(Some(state), {
                  createCheckbox(state.items, item)(parentCheckboxEvent _)
                }, {
                  // check if the  selected item is used as a key for els in State
                  val elements = if (state.els.filterKeys(_ == state.selected).isEmpty) Vector.empty else state.els(state.selected)
                  getChildrenEls(item == state.selected && state.items(item).isOpened)(elements)
                })
                /** end **/
              }.toList
            )
          )
        )
    }
    ,
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

    def getText: String = getText(item)

    /**
      *
      * @param item
      * @return
      */
    private def isChecked(item: Item): Boolean = item match {
      case t: TreeItem => t.checked
      case l: LeafItem => l.checked
    }

    /**
      *
      * @return
      */
    def getCheckedValue: Boolean = isChecked(item)

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
  def traverse(cv: ChildView, state: State): Option[ChildView] = {
    def iter(names: Seq[String], vs: Vector[ChildView], acc: Seq[ChildView]): Seq[ChildView] = {

      if (names.isEmpty) return acc

      val res: Option[(String, ChildView, Vector[ChildView])] = vs.map {
        case c: ChildView => c.item match {
          case l: LeafItem => (l.text, c, Vector.empty[ChildView])
          case t: TreeItem => (t.text, c, t.items)
        }
      }.filter { case (n, _, _) => n == names.head }.headOption

      // visit node by depth
      if (res.isDefined) {
        val (_, curr, children) = res.get
        (curr.getText == cv.getText) match {
          case true => iter(names.tail, children, acc :+ cv)
          case _ => iter(names.tail, children, acc :+ curr)
        }
      } else acc
    }

    /**
      *
      * @param structure
      * @return
      */
    def compose(structure: Seq[ChildView]): Option[ChildView] = {
      val (optChildView, _) = structure.foldRight((Option.empty[ChildView], false)) { (i, pair) =>
        val (result, state) = pair
        if (!result.isDefined) (Some(i), i.getCheckedValue)
        else {
          val updated: ChildView = i.item match {
            case t: TreeItem =>
              val up: Vector[ChildView] = t.items.map { sub =>
                if (sub.getText == result.get.getText) result.get else sub
              }
              //              val pState = !(t.items.count(_.getCheckedValue == false) > 0)
              i.copy(item = t.copy(/*checked = pState,*/ items = up))
            case l: LeafItem if (l.text == result.get.getText) =>
              i.copy(item = result.get.item)
            case _ => i
          }
          (Some(updated), state)
        }
      }

      // result
      optChildView
    }

    // index 0 refers to root parent..
    val (root, remains) = cv.genealogy match {
      case h :: tail => (h, tail)
    }

    //    println(s"GENEALOGY ${cv.genealogy}")
    //    println("-----------------------------")
    val accumulated = iter(remains, state.items(root).tree.items, Seq.empty[ChildView])

    // updated view
    compose(accumulated)
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