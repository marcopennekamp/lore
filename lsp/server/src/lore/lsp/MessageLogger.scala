package lore.lsp

import org.eclipse.lsp4j.{MessageParams, MessageType}
import org.eclipse.lsp4j.services.LanguageClient

/**
  * Use this class to send log messages to the client.
  *
  * DO NOT use `println` when a request is processed, as that will send a response to the language client, invalidating
  * the actual response that follows.
  */
object MessageLogger {

  def log(message: String)(implicit client: LanguageClient): Unit = log(MessageType.Log, message)
  def info(message: String)(implicit client: LanguageClient): Unit = log(MessageType.Info, message)
  def warn(message: String)(implicit client: LanguageClient): Unit = log(MessageType.Warning, message)
  def error(message: String)(implicit client: LanguageClient): Unit = log(MessageType.Error, message)

  private def log(messageType: MessageType, message: String)(implicit client: LanguageClient): Unit = {
    client.logMessage(new MessageParams(messageType, message))
  }

}
