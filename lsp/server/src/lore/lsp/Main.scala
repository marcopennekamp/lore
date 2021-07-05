package lore.lsp

import org.eclipse.lsp4j.launch.LSPLauncher

object Main {

  def main(args: Array[String]): Unit = {
    val server = new LoreLanguageServer
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    val client = launcher.getRemoteProxy

    server.connect(client)
    launcher.startListening()
  }

}
