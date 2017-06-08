import java.util.UUID

import View.{View, TreeView, ChildView}

/**
  * Created by jferrolino on 28/5/17.
  */

object Item {

  sealed trait Item

  /**
    * Represents a tree item
    *
    * @param text
    * @param checked
    * @param items
    * @param id
    */
  case class TreeItem(text: String, checked: Boolean = false,
                      items: Vector[ChildView] = Vector.empty, id: UUID = UUID.randomUUID()) extends Item

  /**
    * Represents a leaf item
    *
    * @param text
    * @param checked
    * @param id
    */
  case class LeafItem(text: String,
                      checked: Boolean = false,
                      id: UUID = UUID.randomUUID()) extends Item


  /**
    * For providing default data (items) to tree
    *
    * @param n
    * @param tv
    * @return
    */
  def apply(n: Int, tv: Option[View] = None, ids: Seq[UUID] = Nil): Vector[ChildView] = {
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
          val nestedItem = TreeItem(text = s"Nested Tree #$l")
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

