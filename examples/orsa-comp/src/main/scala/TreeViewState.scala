import java.util.UUID

import View.TreeView
import korolev.{Effects, VDom}
import korolev.execution.{defaultExecutor, defaultScheduler}

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

case class TreeViewState(
                          itemSelected: Option[UUID] = None,
                          items: Map[String, TreeView],
                          els: Map[String, Vector[VDom.Node]])

object TreeViewState {

  //val effects: Effects[Future, TreeViewState, Any] = Effects[Future, TreeViewState, Any]

  def apply(): TreeViewState = TreeViewState()
}
