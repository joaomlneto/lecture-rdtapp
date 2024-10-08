package rdtapp.helpers

import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.Input
import reactives.default.*
import reactives.operator.Event.CBR
import scalatags.JsDom.all.*
import scalatags.JsDom.{Attr, TypedTag}

object RenderUtil {
  def inputFieldHandler(
      tag: TypedTag[Input],
      attr: Attr,
      clear: Boolean = true
  ): CBR[String, org.scalajs.dom.html.Input] = {
    val handler               = Event.fromCallback[TypedTag[Input], UIEvent](tag(attr := Event.handle))
    val todoInputField: Input = handler.data.render

    // observer to prevent form submit and empty content
    handler.event.observe { (e: UIEvent) =>
      e.preventDefault()
      if clear then todoInputField.value = ""
    }

    // note that the accessed value is NOT a reactive, there is a name clash with the JS library :-)
    val inputFieldText = handler.event.map { _ => todoInputField.value }

    CBR(inputFieldText, todoInputField)
  }
}
