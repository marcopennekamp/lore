package lore.compiler.utils

import org.slf4j.LoggerFactory
import org.slf4j.helpers.SubstituteLogger

/**
  * An indentation logger indents its <b>trace</b> messages with the current indentation. This can be used to provide
  * visual clarity to nested relationships.
  *
  * `eventQueue` can be `null` if `createdPostInitialization` is `true`, at least according to the code.
  */
case class IndentationLogger(name: String, step: Int = 4) extends SubstituteLogger(name, null, true) {
  setDelegate(LoggerFactory.getLogger(name))

  private var indentation: Int = 0

  def indent(): Unit = indentation += step
  def dedent(): Unit = indentation -= step
  def indented[R](block: => R): R = {
    indent()
    val result = block
    dedent()
    result
  }

  override def trace(msg: String): Unit = super.trace(addIndentation(msg))
  override def trace(format: String, arg: Any): Unit = super.trace(addIndentation(format), arg)
  override def trace(format: String, arg1: Any, arg2: Any): Unit = super.trace(addIndentation(format), arg1, arg2)
  override def trace(format: String, arguments: Object*): Unit = super.trace(addIndentation(format), arguments: _*)
  override def trace(msg: String, t: Throwable): Unit = super.trace(addIndentation(msg), t)

  private def addIndentation(msg: String): String = " ".repeat(indentation) + msg
}
