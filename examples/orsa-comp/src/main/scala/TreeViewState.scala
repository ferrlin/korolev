import java.util.UUID

import View.TreeView
import korolev.{Effects, VDom}
import korolev.execution.{defaultExecutor, defaultScheduler}

import scala.concurrent.Future

/**
  * Created by jferrolino on 28/5/17.
  */
case class TreeViewState(rootSelected: String,
                         itemSelected: Option[UUID] = None,
                         items: Map[String, TreeView],
                         els: Map[String, Vector[VDom.Node]])

object TreeViewState {

  val effects: Effects[Future, TreeViewState, Any] = Effects[Future, TreeViewState, Any]

  def apply(): TreeViewState = TreeViewState()
}
