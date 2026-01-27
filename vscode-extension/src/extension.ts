import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Executable,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverExecutable: Executable = {
    command: path.join(context.extensionPath, '..', 'target', 'release', 'bagel-language-server'),
    args: [],
  };

  const serverOptions: ServerOptions = {
    run: serverExecutable,
    debug: serverExecutable,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'bagel' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.bgl'),
    },
  };

  client = new LanguageClient(
    'bagelLanguageServer',
    'Bagel Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
