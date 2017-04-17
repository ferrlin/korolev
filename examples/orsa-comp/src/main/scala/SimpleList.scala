import korolev._
import korolev.server._
import korolev.blazeServer._
import korolev.execution._

import scala.concurrent.Future

object SimpleList extends KorolevBlazeServer {

  import State.effects._

  // Handler to input
  val inputId = elementId

  val service = blazeService[Future, State, Any] from KorolevServiceConfig[Future, State, Any](
    serverRouter = ServerRouter.empty[Future, State],
    stateStorage = StateStorage.default(State()),
    head = 'head (
      'title ("Simple Treeview Page"),
      'link (
        'href /= "/treeview.css",
        'rel /= "stylesheet",
        'type /= "text/css")
    ),
    render = {
      case state =>
        'body (
          'div ('style /= "height: 250px; overflow-y: scroll",
            (state.todos zipWithIndex) map {
              case (todo, i) =>
                'div (
                  'input (
                    'type /= "checkbox",
                    'checked when todo.done,
                    // Generate transition when clicking checkboxes
                    event('click) {
                      immediateTransition { case tState =>
                        val updated = tState.todos.updated(i, tState.todos(i).copy(done = !todo.done))
                        tState.copy(todos = updated)
                      }
                    }
                  ),
                  'span (todo.text)
                )
            }
          )
        )
    }
  )
}

case class State(todos: Vector[State.Todo] = (0 to 4).toVector map {
  i => State.Todo(s"This is a simple list #$i", done = false)
})

object State {
  val effects = Effects[Future, State, Any]

  case class Todo(text: String, done: Boolean)

}