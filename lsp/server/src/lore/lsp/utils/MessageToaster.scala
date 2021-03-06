package lore.lsp.utils

import lore.lsp.LanguageServerContext
import org.eclipse.lsp4j.{MessageParams, MessageType}

/**
  * MessageToaster allows showing message toasters to the client. This directly reaches the language user in the GUI.
  */
object MessageToaster {

  def info(message: String)(implicit context: LanguageServerContext): Unit = show(MessageType.Info, message)

  private def show(messageType: MessageType, message: String)(implicit context: LanguageServerContext): Unit = {
    context.client.showMessage(new MessageParams(messageType, s"Lore: $message"))
  }

}
