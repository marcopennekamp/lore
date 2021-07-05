import * as path from 'path'
import * as vscode from 'vscode'
import { Executable, LanguageClient, LanguageClientOptions } from 'vscode-languageclient/node'

let client: LanguageClient

/**
 * The extension is activated when the user opens a workspace that contains a `lore.build.json` file.
 */
export function activate(context: vscode.ExtensionContext) {
  console.log('Congratulations, your extension "lore" is now active!')

  let serverExecutable: Executable = { command: 'java', args: ['-jar', context.asAbsolutePath(path.join('out', 'lore-language-server.jar'))] }
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'lore' }],
    synchronize: {
      // Notify the server about file changes to `.lore` files contained in the workspace.
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.lore'),
    },
  }

  client = new LanguageClient(
    'lore-language-server',
    'Lore Language Server',
    serverExecutable,
    clientOptions,
  )
  client.start()

  client.onReady().then(() => {
    console.log(client.initializeResult)
  })
}

export function deactivate() {
  if (!client) {
    return undefined
  }
  return client.stop()
}
