const vscode = require('vscode');
const cp = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { LspClient } = require('./lsp-client');

const KEYWORDS = [
  'rule',
  'set',
  'put',
  'yield',
  'when',
  'otherwise',
  'repeat',
  'while',
  'end',
  'import',
  'book',
  'field',
  'new'
];

const BUILTIN_MODULES = [
  'std::string',
  'std::bytes',
  'std::io',
  'std::time',
  'std::fs',
  'std::path',
  'std::env',
];
const SEMANTIC_TOKEN_TYPES = [
  'keyword',
  'type',
  'function',
  'method',
  'class',
  'property',
  'parameter',
  'variable',
  'string',
  'number',
  'operator'
];
const SEMANTIC_TOKEN_MODIFIERS = [];

function activate(context) {
  const output = vscode.window.createOutputChannel('BirdDisk');
  const diagnostics = vscode.languages.createDiagnosticCollection('birddisk');
  const config = vscode.workspace.getConfiguration('birddisk');
  const enableLsp = config.get('enableLsp', false);
  const lspPath = config.get('lspPath', 'birddisk-lsp');
  const lspClient = enableLsp ? new LspClient(output, diagnostics) : null;
  const lspRunning = lspClient ? lspClient.start(lspPath, workspaceRootUri()) : false;

  const formatProvider = vscode.languages.registerDocumentFormattingEditProvider(
    { language: 'birddisk' },
    {
      provideDocumentFormattingEdits(document) {
        return formatDocument(document, output);
      }
    }
  );
  const subscriptions = [formatProvider, diagnostics, output];

  if (lspRunning) {
    const semanticLegend = new vscode.SemanticTokensLegend(
      SEMANTIC_TOKEN_TYPES,
      SEMANTIC_TOKEN_MODIFIERS
    );
    const semanticProvider = vscode.languages.registerDocumentSemanticTokensProvider(
      { language: 'birddisk' },
      {
        provideDocumentSemanticTokens(document) {
          return lspClient.requestSemanticTokens(document).then((data) => {
            if (!data) {
              return null;
            }
            return new vscode.SemanticTokens(new Uint32Array(data));
          });
        }
      },
      semanticLegend
    );

    const inlayProvider = vscode.languages.registerInlayHintsProvider(
      { language: 'birddisk' },
      {
        provideInlayHints(document, range) {
          return lspClient.requestInlayHints(document, range).then((hints) => {
            return hints.map((hint) => lspInlayToVs(hint)).filter((hint) => hint);
          });
        }
      }
    );

    const hoverProvider = vscode.languages.registerHoverProvider(
      { language: 'birddisk' },
      {
        provideHover(document, position) {
          return lspClient.requestHover(document, position);
        }
      }
    );
    const definitionProvider = vscode.languages.registerDefinitionProvider(
      { language: 'birddisk' },
      {
        provideDefinition(document, position) {
          return lspClient.requestDefinition(document, position);
        }
      }
    );
    const renameProvider = vscode.languages.registerRenameProvider(
      { language: 'birddisk' },
      {
        provideRenameEdits(document, position, newName) {
          return lspClient.requestRename(document, position, newName);
        }
      }
    );
    const completionProvider = vscode.languages.registerCompletionItemProvider(
      { language: 'birddisk' },
      {
        provideCompletionItems(document, position) {
          return lspClient.requestCompletion(document, position);
        }
      },
      ':'
    );

    const openListener = vscode.workspace.onDidOpenTextDocument((document) => {
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      lspClient.sendNotification('textDocument/didOpen', {
        textDocument: {
          uri: document.uri.toString(),
          languageId: 'birddisk',
          version: document.version,
          text: document.getText()
        }
      });
    });

    const changeListener = vscode.workspace.onDidChangeTextDocument((event) => {
      const document = event.document;
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      lspClient.sendNotification('textDocument/didChange', {
        textDocument: {
          uri: document.uri.toString(),
          version: document.version
        },
        contentChanges: [{ text: document.getText() }]
      });
    });

    const saveListener = vscode.workspace.onDidSaveTextDocument((document) => {
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      lspClient.sendNotification('textDocument/didSave', {
        textDocument: { uri: document.uri.toString() }
      });
    });

    const closeListener = vscode.workspace.onDidCloseTextDocument((document) => {
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      lspClient.sendNotification('textDocument/didClose', {
        textDocument: { uri: document.uri.toString() }
      });
    });

    for (const doc of vscode.workspace.textDocuments) {
      if (doc.languageId !== 'birddisk' || doc.uri.scheme !== 'file') {
        continue;
      }
      lspClient.sendNotification('textDocument/didOpen', {
        textDocument: {
          uri: doc.uri.toString(),
          languageId: 'birddisk',
          version: doc.version,
          text: doc.getText()
        }
      });
    }

    subscriptions.push(
      semanticProvider,
      inlayProvider,
      hoverProvider,
      definitionProvider,
      renameProvider,
      completionProvider,
      openListener,
      changeListener,
      saveListener,
      closeListener
    );
  } else {
    const completionProvider = vscode.languages.registerCompletionItemProvider(
      { language: 'birddisk' },
      {
        provideCompletionItems(document, position) {
          const line = document.lineAt(position).text;
          const trimmed = line.trimStart();
          const isImport = trimmed.startsWith('import ');
          const items = [];

          if (isImport) {
            for (const moduleName of collectImportModules(document)) {
              const item = new vscode.CompletionItem(moduleName, vscode.CompletionItemKind.Module);
              item.insertText = moduleName.endsWith('.') ? moduleName : `${moduleName}.`;
              item.detail = 'BirdDisk module';
              items.push(item);
            }
            return items;
          }

          for (const keyword of KEYWORDS) {
            const item = new vscode.CompletionItem(keyword, vscode.CompletionItemKind.Keyword);
            items.push(item);
          }
          return items;
        }
      }
    );

    const saveListener = vscode.workspace.onDidSaveTextDocument((document) => {
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      runDiagnostics(document, diagnostics, output);
    });

    const openListener = vscode.workspace.onDidOpenTextDocument((document) => {
      if (document.languageId !== 'birddisk' || document.uri.scheme !== 'file') {
        return;
      }
      runDiagnostics(document, diagnostics, output);
    });

    subscriptions.push(completionProvider, saveListener, openListener);
  }

  context.subscriptions.push(...subscriptions);
}

function deactivate() {}

function collectImportModules(document) {
  const modules = new Set(BUILTIN_MODULES);
  const root = findStdlibRoot(document.uri.fsPath);
  if (root) {
    for (const moduleName of scanStdlibModules(root)) {
      modules.add(moduleName);
    }
  }
  return Array.from(modules).sort();
}

function findStdlibRoot(filePath) {
  let current = path.dirname(filePath);
  while (true) {
    const candidate = path.join(current, 'stdlib');
    if (fs.existsSync(candidate) && fs.statSync(candidate).isDirectory()) {
      return candidate;
    }
    const parent = path.dirname(current);
    if (parent === current) {
      return null;
    }
    current = parent;
  }
}

function scanStdlibModules(root) {
  const modules = [];
  const stack = [root];
  while (stack.length > 0) {
    const dir = stack.pop();
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        stack.push(fullPath);
        continue;
      }
      if (entry.isFile() && entry.name.endsWith('.bd')) {
        const rel = path.relative(root, fullPath);
        const parts = rel.split(path.sep).map((part) => part.replace(/\.bd$/, ''));
        modules.push(parts.join('::'));
      }
    }
  }
  return modules;
}

function runDiagnostics(document, diagnostics, output) {
  const config = vscode.workspace.getConfiguration('birddisk', document.uri);
  if (!config.get('enableDiagnostics', true)) {
    diagnostics.delete(document.uri);
    return;
  }
  const compilerPath = config.get('compilerPath', 'birddiskc');
  const args = ['check', '--json', document.uri.fsPath];
  const cwd = workspaceFolderFor(document.uri);

  cp.execFile(compilerPath, args, { cwd, maxBuffer: 1024 * 1024 }, (err, stdout, stderr) => {
    if (!stdout) {
      if (stderr) {
        output.appendLine(stderr.trim());
      }
      if (err) {
        output.appendLine(`Failed to run '${compilerPath}': ${err.message}`);
      }
      return;
    }
    let report;
    try {
      report = JSON.parse(stdout);
    } catch (parseErr) {
      output.appendLine('Failed to parse birddisk JSON output.');
      output.appendLine(stdout.trim());
      return;
    }
    const list = Array.isArray(report.diagnostics) ? report.diagnostics : [];
    const items = list.map((diag) => mapDiagnostic(diag, document));
    diagnostics.set(document.uri, items);
  });
}

function workspaceFolderFor(uri) {
  const folder = vscode.workspace.getWorkspaceFolder(uri);
  if (folder) {
    return folder.uri.fsPath;
  }
  return path.dirname(uri.fsPath);
}

function mapDiagnostic(diag, document) {
  const start = toPosition(diag && diag.span ? diag.span.start : null);
  const end = toPosition(diag && diag.span ? diag.span.end : null);
  const range = new vscode.Range(start, end);
  const message = diag && diag.message ? diag.message : 'Unknown diagnostic';
  let severity = vscode.DiagnosticSeverity.Error;
  if (diag && diag.severity === 'warning') {
    severity = vscode.DiagnosticSeverity.Warning;
  }
  const diagnostic = new vscode.Diagnostic(range, message, severity);
  if (diag && diag.code) {
    diagnostic.code = diag.code;
  }
  return diagnostic;
}

function lspInlayToVs(hint) {
  if (!hint || !hint.position || typeof hint.label === 'undefined') {
    return null;
  }
  const position = new vscode.Position(hint.position.line, hint.position.character);
  let label = hint.label;
  if (Array.isArray(label)) {
    label = label
      .map((part) => (typeof part === 'string' ? part : part && part.value ? part.value : ''))
      .join('');
  } else if (typeof label !== 'string') {
    label = '';
  }
  const kind =
    hint.kind === 1 ? vscode.InlayHintKind.Type : vscode.InlayHintKind.Parameter;
  const inlay = new vscode.InlayHint(position, label, kind);
  if (hint.paddingLeft) {
    inlay.paddingLeft = true;
  }
  if (hint.paddingRight) {
    inlay.paddingRight = true;
  }
  return inlay;
}

function toPosition(pos) {
  const line = pos && typeof pos.line === 'number' ? pos.line : 1;
  const col = pos && typeof pos.col === 'number' ? pos.col : 1;
  return new vscode.Position(Math.max(line - 1, 0), Math.max(col - 1, 0));
}

function workspaceRootUri() {
  const folders = vscode.workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    return null;
  }
  return folders[0].uri.toString();
}

function formatDocument(document, output) {
  const config = vscode.workspace.getConfiguration('birddisk', document.uri);
  const compilerPath = config.get('compilerPath', 'birddiskc');
  const cwd = workspaceFolderFor(document.uri);
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'birddisk-'));
  const tempPath = path.join(tempDir, path.basename(document.uri.fsPath));
  fs.writeFileSync(tempPath, document.getText(), 'utf8');

  return new Promise((resolve) => {
    cp.execFile(compilerPath, ['fmt', tempPath], { cwd, maxBuffer: 1024 * 1024 }, (err, stdout, stderr) => {
      if (stderr) {
        output.appendLine(stderr.trim());
      }
      if (err) {
        output.appendLine(`Failed to format with '${compilerPath}': ${err.message}`);
        cleanupTemp(tempDir);
        resolve([]);
        return;
      }
      let formatted = '';
      try {
        formatted = fs.readFileSync(tempPath, 'utf8');
      } catch (readErr) {
        output.appendLine(`Failed to read formatted output: ${readErr.message}`);
        cleanupTemp(tempDir);
        resolve([]);
        return;
      }
      cleanupTemp(tempDir);
      resolve([vscode.TextEdit.replace(fullRange(document), formatted)]);
    });
  });
}

function fullRange(document) {
  if (document.lineCount === 0) {
    const pos = new vscode.Position(0, 0);
    return new vscode.Range(pos, pos);
  }
  const last = document.lineAt(document.lineCount - 1);
  return new vscode.Range(new vscode.Position(0, 0), last.range.end);
}

function cleanupTemp(dir) {
  try {
    fs.rmSync(dir, { recursive: true, force: true });
  } catch (_) {
    // Ignore cleanup failures.
  }
}

module.exports = {
  activate,
  deactivate
};
