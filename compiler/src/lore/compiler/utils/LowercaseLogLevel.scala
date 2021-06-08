package lore.compiler.utils

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.pattern.ClassicConverter
import ch.qos.logback.classic.spi.ILoggingEvent
import lore.compiler.feedback.Feedback

class LowercaseLogLevel extends ClassicConverter {
  override def convert(event: ILoggingEvent): String = {
    event.getLevel match {
      case Level.ERROR => Feedback.tagError
      case Level.WARN => Feedback.tagWarning
      case level => level.toString.toLowerCase
    }
  }
}
