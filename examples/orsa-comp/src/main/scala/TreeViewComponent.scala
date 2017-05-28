import java.util.UUID

import Item.{Item, LeafItem, TreeItem}
import View.{ChildView, TreeView, View, traverse}
import korolev._

/**
  * Created by jferrolino on 28/5/17.
  */
object TreeViewComponent {

  import TreeViewState.effects._

  val TREE_STYLE_PLUS = "icon expand-icon glyphicon glyphicon-plus"
  val TREE_STYLE_MINUS = "icon expand-icon glyphicon glyphicon-minus"
  val STYLE_CHECKED = "icon check-icon glyphicon glyphicon-check"
  val STYLE_UNCHECKED = "icon check-icon glyphicon glyphicon-unchecked"
  val STYLE_LIST_ITEM = "list-group-item node-treeview-checkable"
  val STYLE_TREE_ITEM = "list-group-item node-treeview-checkable"

  /**
    * Tree element style when clicked/unclicked (ie '-' or '+')
    *
    * @param isOpened - state where the tree is currently collapsed.
    * @return
    */
  def getTreeStyleClass(isOpened: Boolean, root: Option[TreeItem] = None, nested: Option[ChildView] = None)
                       (t: (TreeItem) => StateManager.Transition[TreeViewState])
                       (n: (Option[ChildView]) => StateManager.Transition[TreeViewState]): VDom.Node =
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
  def createCheckbox(items: Map[String, TreeView], item: String)
                    (t: (String) => StateManager.Transition[TreeViewState]): VDom.Node = {
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
  def createCheckbox(item: Item, parent: String)
                    (t: (String, Boolean) => StateManager.Transition[TreeViewState]): VDom.Node = {
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
  def getStyleFor(name: String, isSelected: Boolean): VDom.Node = {
    if (isSelected) 'strong (name) else 'span (name)
  }

  def getChildrenEls(isSelected: Boolean)(els: Vector[VDom.Node]): Vector[VDom.Node] =
    if (isSelected && els.nonEmpty) els else Vector.empty[VDom.Node]

  /**
    * Recursively updates the checkbox value.
    *
    * @param children
    * @param status
    * @return
    */
  def updateSubElements(children: Vector[ChildView], status: Boolean): Vector[ChildView] =
    children.map {
      case c => c.copy(item = c.item match {
        case t: TreeItem => t.copy(checked = status, items = updateSubElements(t.items, status))
        case l: LeafItem => l.copy(checked = status)
      })
    }

  /**
    *
    * @param reference - targeted view instance
    * @param name      - the text values of view included
    * @param isChecked - flag variable
    * @return
    */
  private def updateReference(reference: View, name: Option[String], isChecked: Boolean): View = reference match {
    case tv@TreeView(opened, treeItem) =>
      tv.copy(tree = treeItem.copy(items = treeItem.items.map {
        case c@ChildView(_, l: LeafItem, _) if name.isDefined && l.text == name.get =>
          c.copy(item = l.copy(checked = !isChecked))
        case c@ChildView(_, t: TreeItem, _) if name.isDefined && t.text == name.get =>
          c.copy(item = t.copy(checked = !isChecked, items = updateSubElements(t.items, !isChecked)))
        case c@ChildView(_, i: Item, _) => c.copy(item = i)
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
  def updatedDeeperItems(ref: ChildView, s: TreeViewState, selected: String): (Boolean, TreeItem) = {
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
  def updateRootValue(root: TreeItem, child: Option[ChildView]): Vector[ChildView] = {
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
    * Extractor of component to generate the virtual DOM node instance.
    *
    * @param state
    * @return
    */
  def apply(state: TreeViewState): VDom.Node = new TreeViewComponent().render(state)
}

class TreeViewComponent {

  import TreeViewComponent._
  import TreeViewState.effects._

  /**
    * Event handler for root tree
    *
    * @return
    */
  def mainTreeEventHandler(target: TreeItem): StateManager.Transition[TreeViewState] = {
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
  def nestedTreeEventHandler(target: Option[ChildView]): StateManager.Transition[TreeViewState] = {
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
  def createTree(item: Option[TreeItem], view: Option[ChildView] = None)(state: Option[TreeViewState], checkbox: VDom.Node, childrenEls: Vector[VDom.Node]): Vector[VDom.Node] = {
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
    ) yield 'li ('class /= STYLE_LIST_ITEM, 'node /= "1", el)

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

  /**
    *
    * @param items * @return
    */
  def generateLI(items: Vector[ChildView], doGenerate: Boolean = true, ref: String)(state: Option[TreeViewState]): Vector[VDom.Node] =
    if (doGenerate) items.flatMap {
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
  def parentCheckboxEvent(item: String): StateManager.Transition[TreeViewState] = {
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
    * Event for children (used in dealing with checkbox clicks event)
    *
    * @param ref
    * @param name
    * @param isChecked
    * @return
    */
  def childCheckboxEvent(ref: View)(name: String, isChecked: Boolean): StateManager.Transition[TreeViewState] = {
    case s: TreeViewState => {
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
  def itemSelectedEventHandler(ref: Option[View]): StateManager.Transition[TreeViewState] = {
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
    * Render the component to it's Virtual DOM  Node instance.
    *
    * @return
    */
  def render: Render[TreeViewState] = {
    case state =>
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
  }
}

