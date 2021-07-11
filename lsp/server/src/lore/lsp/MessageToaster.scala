package lore.lsp

import org.eclipse.lsp4j.{MessageParams, MessageType}
import org.eclipse.lsp4j.services.LanguageClient

/**
  * MessageToaster allows showing message toasters to the client. This directly reaches the language user in the GUI.
  */
object MessageToaster {

  def info(message: String)(implicit client: LanguageClient): Unit = show(MessageType.Info, message)

  private def show(messageType: MessageType, message: String)(implicit client: LanguageClient): Unit = {
    client.showMessage(new MessageParams(messageType, s"Lore: $message"))
  }

}
