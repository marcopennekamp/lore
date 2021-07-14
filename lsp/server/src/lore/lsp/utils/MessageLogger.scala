package lore.lsp.utils

import lore.compiler.feedback.{LambdaReporter, Reporter}
import lore.lsp.LanguageServerContext
import org.eclipse.lsp4j.{MessageParams, MessageType}

/**
  * MessageLogger allows sending log messages to the client.
  *
  * DO NOT use `println` when a request is processed, as that will send a response to the language client, invalidating
  * the actual response that follows.
  */
object MessageLogger {

  def log(message: String)(implicit context: LanguageServerContext): Unit = log(MessageType.Log, message)

  def info(message: String)(implicit context: LanguageServerContext): Unit = log(MessageType.Info, message)

  def warn(message: String)(implicit context: LanguageServerContext): Unit = log(MessageType.Warning, message)

  def error(message: String)(implicit context: LanguageServerContext): Unit = log(MessageType.Error, message)

  def freshReporter(implicit context: LanguageServerContext): Reporter = new LambdaReporter(feedback => error(feedback.toString))

  private def log(messageType: MessageType, message: String)(implicit context: LanguageServerContext): Unit = {
    context.client.logMessage(new MessageParams(messageType, message))
  }

}
