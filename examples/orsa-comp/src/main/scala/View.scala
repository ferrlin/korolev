import java.util.UUID

import Item.{Item, LeafItem, TreeItem}

import scala.annotation.tailrec

/**
  * Created by jferrolino on 28/5/17.
  */

object View {

  val IS_OPENED = false

  sealed trait View

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
  def traverse(cv: ChildView, state: TreeViewState): Option[ChildView] = {
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
