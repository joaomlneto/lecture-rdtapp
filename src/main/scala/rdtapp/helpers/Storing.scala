package rdtapp.helpers

import com.github.plokhotnyuk.jsoniter_scala.core.*
import org.scalajs.dom
import reactives.default.Signal

object Storing {

  def storedAs[A: JsonValueCodec](key: String, default: => A)(create: A => Signal[A]): Signal[A] = {
    val init: A =
      try { readFromString[A](dom.window.localStorage.getItem(key)) }
      catch {
        case _: Throwable =>
          dom.window.localStorage.removeItem(key)
          default
      }
    val sig = create(init)
    sig.observe(
      { (ft: A) =>
        dom.window.localStorage.setItem(key, writeToString(ft))
      },
      fireImmediately = false
    )
    sig
  }

}
