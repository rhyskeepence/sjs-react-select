package rhyskeepence.example

import japgolly.scalajs.react.{ReactElement, React}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import rhyskeepence.Select

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object Example extends JSApp {

  case class State(value: String, label: String)

  val allStates = List(
    State("australian-capital-territory", "Australian Capital Territory" ),
    State("new-south-wales", "New South Wales" ),
    State("victoria", "Victoria" ),
    State("queensland", "Queensland" ),
    State("western-australia", "Western Australia" ),
    State("south-australia", "South Australia" ),
    State("tasmania", "Tasmania" ),
    State("northern-territory", "Northern Territory")
  )

  @JSExport
  def main(): Unit = {

    val select = Select[State](
      selectClass         = "example",
      placeholder         = "Select a movie",
      initialValue        = None,
      values              = allStates,
      query               = query => allStates.filter(_.label.toLowerCase.contains(query.toLowerCase)),
      onChange            = _ => Unit,
      renderTitle         = _.label,
      renderValue         = state => <.span(state.label),
      keyOf               = _.label)

    React.render(select, dom.document.getElementById("example"))
  }

}
