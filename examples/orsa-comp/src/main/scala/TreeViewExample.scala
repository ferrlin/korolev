import java.util.UUID

import State._
import View._
import korolev._
import korolev.blazeServer._
import korolev.execution._
import korolev.server._

import scala.annotation.tailrec
import scala.concurrent.Future

object TreeViewExample extends KorolevBlazeServer {

  import State.effects._

  val TREE_STYLE_PLUS = "icon expand-icon glyphicon glyphicon-plus"
  val TREE_STYLE_MINUS = "icon expand-icon glyphicon glyphicon-minus"

  val STYLE_CHECKED = "icon check-icon glyphicon glyphicon-check"
  val STYLE_UNCHECKED = "icon check-icon glyphicon glyphicon-unchecked"

  val STYLE_LIST_ITEM = "list-group-item node-treeview-checkable"
  val STYLE_TREE_ITEM = "list-group-item node-treeview-checkable"

  // Handler to input
  val inputId: Effects.ElementId = elementId
  val storage: StateStorage[Future, State] = StateStorage.default[Future, State](State())

  /**
    * Tree element style when clicked/unclicked (ie '-' or '+')
    *
    * @param isOpened - state where the tree is currently collapsed.
    * @return
    */
  private def getTreeStyleClass(isOpened: Boolean, root: Option[TreeItem] = None, nested: Option[ChildView] = None)
                               (t: (TreeItem) => StateManager.Transition[State])
                               (n: (Option[ChildView]) => StateManager.Transition[State]): VDom.Node =
    'span ('class /= (if (isOpened) TREE_STYLE_MINUS else TREE_STYLE_PLUS),
      event('click, EventPhase.AtTarget)(immediateTransition(if (nested.isDefined) n(nested) else t(root.get)))
    )

  /**
    *
    * Create a span object for checkboxes
    *
    * @param items - state items that refer to root trees
    * @param item  - component to create a checkbox for
    * @param t     - transition function for dealing with event such as when component is clicked
    * @return
    */
  private def createCheckbox(items: Map[String, TreeView], item: String)
                            (t: (String) => StateManager.Transition[State]): VDom.Node = {
    'span ('class /= (if (items(item).tree.checked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click)(immediateTransition(t(item)))
    )
  }

  /**
    * Overloaded method for creating checkboxes used in children elements
    *
    * @param item
    * @return
    */
  private def createCheckbox(item: State.Item, parent: String)
                            (t: (String, Boolean) => StateManager.Transition[State]): VDom.Node = {
    val (isChecked, name) = item match {
      case l: LeafItem => (l.checked, l.text)
      case t: TreeItem => (t.checked, t.text)
    }

    'span ('class /= (if (isChecked) STYLE_CHECKED else STYLE_UNCHECKED),
      event('click)(immediateTransition(t(name, isChecked)))
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
  private def generateLI(items: Vector[ChildView], doGenerate: Boolean = true, ref: String)(state: Option[State]): Vector[VDom.Node] =
    if (doGenerate)
      items.flatMap {
        case child@ChildView(_, item: LeafItem, parent) => Seq(
          'li ('class /= STYLE_LIST_ITEM,
            createCheckbox(item, ref)(childCheckboxEvent(child) _), {
              val isSelected = state match {
                case None => false
                case Some(s) => s.itemSelected.exists(_.equals(item.id))
              }
              'span (
                event('click, EventPhase.Bubbling) {
                  immediateTransition {
                    itemSelectedEventHandler(Some(child))
                  }
                }, getStyleFor(item.text, isSelected))
            }
          ))
        case child@ChildView(isOpened, item: TreeItem, Some(parent)) => {
          val toUse: View = if (parent.isInstanceOf[TreeView]) parent else child
          // use None since this is used to create a child/nested tree.
          createTree(None, Some(child))(state,
            createCheckbox(item, ref)(childCheckboxEvent(toUse) _),
            generateLI(item.items, isOpened, ref)(state))
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
      val updatedEl = s.els + (item -> generateLI(updatedTree.items, ref = ref.text)(Some(s)))
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
    * Event for children (used in dealing with checkbox clicks event)
    *
    * @param ref
    * @param name
    * @param isChecked
    * @return
    */
  def childCheckboxEvent(ref: View)(name: String, isChecked: Boolean): StateManager.Transition[State] = {
    case s: State => {
      val selected = ref match {
        case tv: TreeView => tv.tree.text
        case cv: ChildView => {
          cv.item match {
            case ti: TreeItem => ti.text
            case ci: LeafItem => ci.text
          }
        }
      }

      //if ref is a tree view.
      val (treeMap, elements) = if (s.items.keySet.contains(selected)) {
        val reference@TreeView(opened, treeItem) = updateReference(s.items(selected), Some(name), isChecked).asInstanceOf[TreeView]
        val updatedEls = s.els + (selected -> generateLI(treeItem.items, ref = selected)(Some(s)))
        (selected -> reference, updatedEls)
      } else {
        // when an item clicked is nested deeper
        val childRef = ref.asInstanceOf[ChildView]
        val selected = childRef.genealogy.head
        val reference: ChildView = updateReference(childRef, None, isChecked).asInstanceOf[ChildView]
        val (opened, updatedParentRef) = updatedDeeperItems(reference, s, selected)
        val lis: Vector[VDom.Node] = generateLI(updatedParentRef.items, ref = selected)(Some(s))
        val updatedEls: Map[String, Vector[VDom.Node]] = s.els + (selected -> lis)
        (selected -> TreeView(opened, updatedParentRef), updatedEls)
      }

      // updated the state with the changes in checkbox value
      s.copy(items = s.items + treeMap, els = elements)
    }
  }

  /**
    * Event handler when an item is selected or clicked.
    * This will highlight the said item for distinction. Future supported
    * behavior will be defined in this handler.
    *
    * @param ref - the item component view where the event originated
    * @return
    */
  def itemSelectedEventHandler(ref: Option[View]): StateManager.Transition[State] = {
    case state =>
      require(ref.isDefined, "Reference should be defined.")
      //TODO: use the id instead of value text
      val (itemSelected, isChecked) = ref match {
        case Some(tv: TreeView) => (tv.tree.id, tv.tree.checked)
        case Some(cv: ChildView) => cv.item match {
          case t: TreeItem => (t.id, t.checked)
          case c: LeafItem => (c.id, c.checked)
        }
        // worst case
        case _ => (null, false)
      }

      // set the item as the selected one
      val updatedState = state.copy(itemSelected = Some(itemSelected))

      //update the html elements highlighting the selected item
      val elements = ref match {
        case Some(tv: TreeView) =>
          //parent
          val root = tv.tree.text
          val lis: Vector[VDom.Node] = generateLI(tv.tree.items, ref = root)(Some(updatedState))
          /*val updatedEls: Map[String, Vector[VDom.Node]] =*/ state.els + (root -> lis)
        case Some(cv: ChildView) =>
          val root = cv.genealogy.head
          val (opened, updatedParentRef) = updatedDeeperItems(cv, state, root)
          val lis: Vector[VDom.Node] = generateLI(updatedParentRef.items, ref = root)(Some(updatedState))
          /*val updatedEls: Map[String, Vector[VDom.Node]] = */ state.els + (root -> lis)
        case None => state.els
      }

      // set the updated elements to the state.
      updatedState.copy(els = elements)
  }

  /**
    *
    * @param reference
    * @param name
    * @param isChecked
    * @return
    */
  private def updateReference(reference: View, name: Option[String], isChecked: Boolean): View = reference match {
    case tv@TreeView(opened, treeItem) =>
      tv.copy(tree = treeItem.copy(items = treeItem.items.map {
        case c@ChildView(_, l: LeafItem, _) if name.isDefined && l.text == name.get =>
          c.copy(item = l.copy(checked = !isChecked))
        case c@ChildView(_, t: TreeItem, _) if name.isDefined && t.text == name.get =>
          c.copy(item = t.copy(checked = !isChecked, items = updateSubElements(t.items, !isChecked)))
        case c@ChildView(_, i: State.Item, _) => c.copy(item = i)
      }))
    case cv: ChildView =>
      cv.copy(item = cv.item match {
        case l: LeafItem => l.copy(checked = !isChecked)
        case t: TreeItem => t.copy(checked = !isChecked, items = updateSubElements(t.items, !isChecked))
      })
  }

  /**
    * Updating item deep in the tree hierarchy.
    *
    * @param ref
    * @param s
    * @param selected
    * @return
    */
  private def updatedDeeperItems(ref: ChildView, s: State, selected: String): (Boolean, TreeItem) = {
    val updatedItem: Option[ChildView] = traverse(ref, s)
    val TreeView(opened, parentRef) = s.items(selected)
    val updatedItems = updateRootValue(parentRef, updatedItem)

    // return the updated parent reference
    (opened, parentRef.copy(items = updatedItems))
  }


  /**
    *
    * @param root  - tree item from root tree
    * @param child - nested child item
    * @return
    */
  private def updateRootValue(root: TreeItem, child: Option[ChildView]): Vector[ChildView] = {
    require(child.isDefined, "Child should be defined.")
    val Some((childText: String, childId: UUID)) = child.map(i => (i.getText, i.getId))
    // updated the root (treeview) with the updated view
    root.items.map { old =>
      old.item match {
        case t: TreeItem if (childId == t.id) => child.get
        case l: LeafItem if (l.id == childId) => child.get
        case _ => old
      }
    }
  }

  /**
    * Event handler for root tree
    *
    * @return
    */
  def mainTreeEventHandler(target: TreeItem): StateManager.Transition[State] = {
    case s =>
      val selected = target.text
      val TreeView(isOpened, els) = s.items(selected)

      // reset other 'opened' state to false
      val other = s.items.filter(_._1 != selected).map(p => (p._1, p._2.copy(isOpened = false)))
      val updatedItems = (s.items ++ other) + (selected -> TreeView(!isOpened, els))

      val lis = generateLI(updatedItems(selected).tree.items, ref = selected)(Some(s))
      val updatedEl = s.els + (selected -> lis)
      s.copy(rootSelected = selected, itemSelected = Some(target.id), items = updatedItems, els = updatedEl)
  }

  /**
    * Event handler for nested tree
    *
    * @return
    */
  def nestedTreeEventHandler(target: Option[ChildView]): StateManager.Transition[State] = {
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
      val lis = generateLI(updatedState(selected).tree.items, ref = selected)(Some(s))
      val updatedEl = s.els + (selected -> lis)
      s.copy(itemSelected = Some(cv.getId), items = updatedState, els = updatedEl)
  }

  /**
    * Creates the tree item with its event handling feature.
    *
    * @param item  -  tree item used for creating root tree view
    * @param view  - for nested tree
    * @param state - reference to the state of the compoment
    * @return
    */
  def createTree(item: Option[TreeItem], view: Option[ChildView] = None)(state: Option[State], checkbox: VDom.Node, childrenEls: Vector[VDom.Node]): Vector[VDom.Node] = {
    val (isSelected, isOpened, text) = if (state.isDefined && item.isDefined && state.get.items.contains(item.get.text)) {
      val treeView = state.get.items(item.get.text)
      val isSelected = state.get.itemSelected.exists(_.equals(item.get.id))
      (isSelected, treeView.isOpened, item.get.text)
    }
    else if (view.isDefined) {
      val isSelected = state.get.itemSelected.exists(_.equals(view.get.getId))
      (isSelected, view.get.isOpened, view.get.getText)
    }
    else (false, false, "")

    val items: Vector[VDom.Node] = for (
      el <- childrenEls
    ) yield 'li ('class /= STYLE_LIST_ITEM, el)

    Vector('li ('class /= STYLE_TREE_ITEM,
      getTreeStyleClass(isOpened, item, view)(mainTreeEventHandler _)(nestedTreeEventHandler _),
      checkbox,
      'span (
        event('click, EventPhase.Bubbling) {
          immediateTransition {
            if (view.isDefined) itemSelectedEventHandler(view)
            else itemSelectedEventHandler(Some(state.get.items(text)))
          }
        }, getStyleFor(text, isSelected /*&& isOpened*/)),
      items)
    ) // ++ items
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
          )
        )
    }
    ,
    serverRouter = {
      ServerRouter(
        dynamic = (_, _) => Router(
          fromState = {
            case State(tab, _, _, _) =>
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
}

case class State(rootSelected: String = State.utv01.tree.text,
                 itemSelected: Option[UUID] = None,
                 items: Map[String, TreeView] = Map(
                   State.utv01.tree.text -> State.utv01,
                   State.utv02.tree.text -> State.utv02,
                   State.utv03.tree.text -> State.utv03),
                 els: Map[String, Vector[VDom.Node]] = Map.empty)

object View {

  import State._

  sealed trait View

  // configurable variables.
  val IS_OPENED = false

  case class TreeView(isOpened: Boolean = false, tree: TreeItem) extends View

  /**
    * isOpened is defaulted to false for non
    * tree child view (ie Leaf).
    *
    */
  case class ChildView(isOpened: Boolean = IS_OPENED, item: Item, parent: Option[View]) extends View {

    // [hack] added sorting  to make more than 3 level deep work
    lazy val genealogy: Seq[String] = getAncestry(parent, Seq.empty[String]) :+ this.getText
    lazy val depth: Int = genealogy.length

    /**
      * The value of the item in string type
      *
      * @return
      */
    def getText: String = getValueText(item)

    /**
      * The id of the item for reference
      *
      * @return
      */
    def getId: UUID = getUUID(item)


    /**
      *
      * @return
      */
    def getCheckedValue: Boolean = isChecked(item)

    /**
      * Get the ancestry of this child view.
      */
    private def getAncestry(parent: Option[View], seq: Seq[String]): Seq[String] = if (parent.isDefined) {
      parent.get match {
        case c: ChildView =>
          // TODO: use id instead of its value text
          val name: String = getValueText(c.item)
          getAncestry(c.parent, name +: seq)
        case t: TreeView => t.tree.text +: seq
      }
    } else Nil

  }

  /** Utility methods **/

  def getValueText(item: Item): String = item match {
    case i: TreeItem => i.text
    case l: LeafItem => l.text
  }

  private def getUUID(item: Item): UUID = item match {
    case i: TreeItem => i.id
    case l: LeafItem => l.id
  }

  private def isChecked(item: Item): Boolean = item match {
    case t: TreeItem => t.checked
    case l: LeafItem => l.checked
  }

  /**
    * Convenience method for traversing a tree with depth first
    * traversal
    *
    * @param cv - target view to
    * @param state
    * @return
    */
  def traverse(cv: ChildView, state: State): Option[ChildView] = {
    @tailrec
    def iter(names: Seq[String], vs: Vector[ChildView], acc: Seq[ChildView]): Seq[ChildView] = {

      if (names.isEmpty) return acc

      val result: Option[(String, ChildView, Vector[ChildView])] = vs.map { c =>
        c.item match {
          case l: LeafItem => (l.text, c, Vector.empty[ChildView])
          case t: TreeItem => (t.text, c, t.items)
        }
      }.find { case (n, _, _) => n == names.head }
      //filter { case (n, _, _) => n == names.head }.headOption

      // visit node by depth first
      if (result.isDefined) {
        val (_, curr, children) = result.get
        curr.getText == cv.getText match {
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


/** ** Initialize state values and model definition ** **/
object State {
  val effects: Effects[Future, State, Any] = Effects[Future, State, Any]

  sealed trait Item

  case class TreeItem(text: String, checked: Boolean = false,
                      items: Vector[ChildView] = Vector.empty, id: UUID = UUID.randomUUID()) extends Item

  case class LeafItem(text: String,
                      checked: Boolean = false,
                      id: UUID = UUID.randomUUID()) extends Item

  object Item {

    def apply(n: Int, tv: Option[View] = None): Vector[ChildView] = {
      var nestedParent: Option[ChildView] = Option.empty[ChildView]
      val nested: Option[ChildView] = (1 to 5).toList.map { i =>
        nestedParent = Some(childTree(i, if (nestedParent.isDefined) nestedParent else tv))
        nestedParent.get
      }.foldRight(Option.empty[ChildView]) { (i, cv) =>
        if (cv.isEmpty) {
          val item = i.item.asInstanceOf[TreeItem]
          val up = item.copy(items = Vector(childLeaf(1, Some(i))))
          Some(i.copy(item = up))
        } else {
          val item = i.item.asInstanceOf[TreeItem]
          val up = item.copy(items = Vector(cv.get))
          //          val upV = i.copy(item = up)
          //          val withParent = cv.get.copy(parent = Some(upV))
          Some(i.copy(item = up))
        }
      }

      (0 to n).toVector.map {
        l => {
          if (l < 2) {
            val nestedItem = TreeItem(s"Nested Tree #$l")
            val nestedTree = ChildView(item = nestedItem, parent = tv)

            val childrenViews = (0 to 2).toVector.map { i =>
              ChildView(item = LeafItem(s"Nested Item ##$i"), parent = Some(nestedTree))
            }
            nestedTree.copy(item = nestedItem.copy(items = childrenViews))
          } else
            ChildView(item = LeafItem(s"Item #$l"), parent = tv)
        }
      } :+ nested.get
    }

    /**
      * Utility method to create child Tre
      *
      * @param count  - item number of the tree
      * @param parent - immediate enclosing component of this tree
      * @return
      */
    def childTree(count: Int, parent: Option[View]): ChildView = {
      val name = parent.get match {
        case t: TreeView => t.tree.text
        case c: ChildView => c.getText
      }
      val tree = TreeItem(s"$name #$count")
      ChildView(item = tree, parent = parent)
    }

    /**
      * Utility method to create child leaves
      *
      * @param count  - item number of the leaf
      * @param parent - immediate enclosing component (ie treeitem) of this leaf
      * @return
      */
    def childLeaf(count: Int, parent: Option[ChildView]): ChildView = {
      val leaf = LeafItem(s"${parent.get.getText} #$count")
      ChildView(item = leaf, parent = parent)
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

  // test..
  //  utv02.tree.items.foreach { cv =>
  //    cv.item match {
  //      case t: TreeItem => t.items.foreach(i => println(s">>> ${i.genealogy} with depth[${i.depth}]"))
  //      case _ => println(s">>> ${cv.genealogy} with depth[${cv.depth}]")
  //    }
  //  }
}