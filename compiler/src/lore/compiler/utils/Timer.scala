package lore.compiler.utils

import lore.compiler.feedback.Feedback

object Timer {

  def timed[R](name: String, n: Int = 1, log: String => Unit = s => Feedback.logger.debug(s))(block: => R): R = {
    val start = System.nanoTime()
    var result: R = block
    for (_ <- 1 until n) {
      result = block
    }
    val end = System.nanoTime()
    log(s"$name took: ${(end - start) / 1000 / n}µs")
    result
  }

}
