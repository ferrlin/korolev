import java.util.UUID

import View.TreeView
import korolev.{Effects, VDom}

import scala.concurrent.Future

/**
  * Created by jferrolino on 28/5/17.
  */
case class TreeViewState(rootSelected: String, //= TreeViewState.utv01.tree.text,
                         itemSelected: Option[UUID] = None,
                         items: Map[String, TreeView] /*=Map(
                           State.utv01.tree.text -> State.utv01,
                           State.utv02.tree.text -> State.utv02,
                           State.utv03.tree.text -> State.utv03)*/ ,
                         els: Map[String, Vector[VDom.Node]] = Map.empty)

object TreeViewState {

  val effects: Effects[Future, TreeViewState, Any] = Effects[Future, TreeViewState, Any]

}
