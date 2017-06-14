import java.util.UUID

import Item.{Item, LeafItem, TreeItem}
import View.{ChildView, TreeView, View}
import korolev.{Effects, VDom}
import korolev.execution.{defaultExecutor, defaultScheduler}

import goggles._
import scala.concurrent.Future

/**
  * Created by jferrolino on 28/5/17.
  */

sealed trait State

case class Initialize(treeViewState: TreeViewState) extends State

case class Execute(treeViewState: TreeViewState) extends State

object State {
  val effects: Effects[Future, State, Any] = Effects[Future, State, Any]
}

case class TreeViewState(itemSelected: Option[UUID] = None,
                         items: Map[String, TreeView],
                         alt: Map[UUID, Map[String, TreeView]] = Map.empty, //
                         els: Map[String, Vector[VDom.Node]])

object TreeViewState {

  def apply(): TreeViewState = TreeViewState()

  /**
    *
    * Convenience method to retrieve all
    * items that are checked.
    *
    * @param state - state used by tree component
    * @return
    */
  def getAllCheckedItems(state: TreeViewState): Set[Item] = {

    /**
      *
      * @param view - any view accepted by tree
      * @param acc  - accumulated items
      * @return
      */
    def getCheckedSingleItem(view: View, acc: Seq[Item]): Seq[Item] =
      view match {
        case t: TreeView if t.tree.checked => getCheckedMultItem(t.tree.items, acc :+ t.tree)
        case t: TreeView => getCheckedMultItem(t.tree.items, acc)
        case c: ChildView => c.item match {
          case ti: TreeItem if ti.checked => getCheckedMultItem(ti.items, acc :+ ti)
          case ti: TreeItem => getCheckedMultItem(ti.items, acc)
          case li: LeafItem if li.checked => acc :+ li
          case li: LeafItem => acc
        }
      }

    /**
      *
      * @param views - item inside a tree
      * @param acc   - accumulated items
      * @return
      */
    def getCheckedMultItem(views: Vector[ChildView], acc: Seq[Item]): Seq[Item] =
      if (views.isEmpty) acc else views.flatMap(getCheckedSingleItem(_, acc))

    // get all items that are checked and returned it as a set.
    state.items.values.flatMap(getCheckedSingleItem(_, Seq.empty)).toSet
  }
}
