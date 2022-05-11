package lore.compiler.utils

import lore.compiler.feedback.Feedback

object Timer {

  sealed trait TimerUnit {
    def format(ns: Long): String
  }

  object TimerUnit {
    case object Milliseconds extends TimerUnit {
      override def format(ns: Long): String = s"${ns / 1000000}ms"
    }

    case object Microseconds extends TimerUnit {
      override def format(ns: Long): String = s"${ns / 1000}µs"
    }

    case object Nanoseconds extends TimerUnit {
      override def format(ns: Long): String = s"${ns}ns"
    }
  }

  def timed[R](
    name: String,
    n: Int = 1,
    log: String => Unit = s => Feedback.logger.debug(s),
    unit: TimerUnit = TimerUnit.Microseconds,
  )(block: => R): R = {
    val start = System.nanoTime()
    var result: R = block
    for (_ <- 1 until n) {
      result = block
    }
    val end = System.nanoTime()
    log(s"$name took: ${unit.format((end - start) / n)}")
    result
  }

}
