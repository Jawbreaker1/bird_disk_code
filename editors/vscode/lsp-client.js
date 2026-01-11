const cp = require('child_process');
const vscode = require('vscode');

class LspClient {
  constructor(output, diagnostics) {
    this.output = output;
    this.diagnostics = diagnostics;
    this.proc = null;
    this.buffer = Buffer.alloc(0);
    this.nextId = 1;
    this.pending = new Map();
    this.ready = false;
    this.pendingNotifications = [];
  }

  start(serverPath, rootUri) {
    try {
      this.proc = cp.spawn(serverPath, [], { stdio: ['pipe', 'pipe', 'pipe'] });
    } catch (err) {
      this.output.appendLine(`Failed to start birddisk-lsp: ${err.message}`);
      this.proc = null;
      return false;
    }

    this.proc.stderr.on('data', (data) => {
      this.output.appendLine(data.toString().trim());
    });

    this.proc.stdout.on('data', (data) => {
      this.buffer = Buffer.concat([this.buffer, data]);
      this.readMessages();
    });

    this.proc.on('error', (err) => {
      this.output.appendLine(`birddisk-lsp error: ${err.message}`);
      this.ready = false;
      this.proc = null;
    });

    this.proc.on('exit', (code) => {
      this.output.appendLine(`birddisk-lsp exited (${code}).`);
      this.ready = false;
    });

    const initParams = {
      processId: process.pid,
      rootUri: rootUri,
      capabilities: {}
    };
    this.sendRequest('initialize', initParams)
      .then(() => {
        this.ready = true;
        this.sendNotification('initialized', {});
        this.flushNotifications();
      })
      .catch((err) => {
        this.output.appendLine(`birddisk-lsp initialize failed: ${err}`);
      });

    return true;
  }

  isRunning() {
    return !!this.proc;
  }

  sendRequest(method, params) {
    if (!this.proc) {
      return Promise.resolve(null);
    }
    const id = this.nextId++;
    const payload = { jsonrpc: '2.0', id, method, params };
    this.writeMessage(payload);
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
    });
  }

  sendNotification(method, params) {
    if (!this.proc) {
      return;
    }
    const payload = { jsonrpc: '2.0', method, params };
    if (this.ready || method === 'initialize') {
      this.writeMessage(payload);
    } else {
      this.pendingNotifications.push(payload);
    }
  }

  flushNotifications() {
    while (this.pendingNotifications.length > 0) {
      const payload = this.pendingNotifications.shift();
      this.writeMessage(payload);
    }
  }

  writeMessage(payload) {
    if (!this.proc) {
      return;
    }
    const body = Buffer.from(JSON.stringify(payload), 'utf8');
    const header = Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, 'utf8');
    this.proc.stdin.write(Buffer.concat([header, body]));
  }

  readMessages() {
    while (true) {
      const headerEnd = this.buffer.indexOf('\r\n\r\n');
      if (headerEnd === -1) {
        return;
      }
      const header = this.buffer.slice(0, headerEnd).toString('utf8');
      const match = header.match(/Content-Length: (\d+)/i);
      if (!match) {
        this.buffer = this.buffer.slice(headerEnd + 4);
        continue;
      }
      const length = parseInt(match[1], 10);
      const total = headerEnd + 4 + length;
      if (this.buffer.length < total) {
        return;
      }
      const body = this.buffer.slice(headerEnd + 4, total);
      this.buffer = this.buffer.slice(total);
      let message;
      try {
        message = JSON.parse(body.toString('utf8'));
      } catch (err) {
        this.output.appendLine('Failed to parse LSP message.');
        continue;
      }
      this.handleMessage(message);
    }
  }

  handleMessage(message) {
    if (typeof message.id !== 'undefined') {
      const entry = this.pending.get(message.id);
      if (entry) {
        this.pending.delete(message.id);
        entry.resolve(message.result);
      }
      return;
    }
    if (message.method === 'textDocument/publishDiagnostics') {
      this.applyDiagnostics(message.params);
    }
  }

  applyDiagnostics(params) {
    if (!params || !params.uri || !Array.isArray(params.diagnostics)) {
      return;
    }
    const uri = vscode.Uri.parse(params.uri);
    const diagnostics = params.diagnostics.map((diag) => lspDiagnosticToVs(diag));
    this.diagnostics.set(uri, diagnostics);
  }

  async requestHover(document, position) {
    if (!this.ready) {
      return null;
    }
    const result = await this.sendRequest('textDocument/hover', {
      textDocument: { uri: document.uri.toString() },
      position: toLspPosition(position)
    });
    if (!result || !result.contents) {
      return null;
    }
    const value = extractHoverText(result.contents);
    if (!value) {
      return null;
    }
    return new vscode.Hover(new vscode.MarkdownString(value));
  }

  async requestDefinition(document, position) {
    if (!this.ready) {
      return null;
    }
    const result = await this.sendRequest('textDocument/definition', {
      textDocument: { uri: document.uri.toString() },
      position: toLspPosition(position)
    });
    if (!result) {
      return null;
    }
    const locations = Array.isArray(result) ? result : [result];
    return locations.map((loc) => lspLocationToVs(loc)).filter((loc) => loc);
  }

  async requestCompletion(document, position) {
    if (!this.ready) {
      return [];
    }
    const result = await this.sendRequest('textDocument/completion', {
      textDocument: { uri: document.uri.toString() },
      position: toLspPosition(position)
    });
    const items = Array.isArray(result) ? result : (result && result.items) || [];
    return items.map((item) => lspCompletionToVs(item));
  }

  async requestRename(document, position, newName) {
    if (!this.ready) {
      return null;
    }
    const result = await this.sendRequest('textDocument/rename', {
      textDocument: { uri: document.uri.toString() },
      position: toLspPosition(position),
      newName
    });
    if (!result || !result.changes) {
      return null;
    }
    const edit = new vscode.WorkspaceEdit();
    for (const [uri, edits] of Object.entries(result.changes)) {
      const target = vscode.Uri.parse(uri);
      for (const change of edits) {
        if (!change.range || typeof change.newText !== 'string') {
          continue;
        }
        edit.replace(target, lspRangeToVs(change.range), change.newText);
      }
    }
    return edit;
  }

  async requestSemanticTokens(document) {
    if (!this.ready) {
      return null;
    }
    const result = await this.sendRequest('textDocument/semanticTokens/full', {
      textDocument: { uri: document.uri.toString() }
    });
    if (!result || !Array.isArray(result.data)) {
      return null;
    }
    return result.data;
  }

  async requestInlayHints(document, range) {
    if (!this.ready) {
      return [];
    }
    const result = await this.sendRequest('textDocument/inlayHint', {
      textDocument: { uri: document.uri.toString() },
      range: toLspRange(range)
    });
    if (!Array.isArray(result)) {
      return [];
    }
    return result;
  }
}

function toLspPosition(position) {
  return { line: position.line, character: position.character };
}

function toLspRange(range) {
  return {
    start: toLspPosition(range.start),
    end: toLspPosition(range.end)
  };
}

function lspRangeToVs(range) {
  const start = new vscode.Position(range.start.line, range.start.character);
  const end = new vscode.Position(range.end.line, range.end.character);
  return new vscode.Range(start, end);
}

function lspLocationToVs(location) {
  if (!location || !location.uri || !location.range) {
    return null;
  }
  const uri = vscode.Uri.parse(location.uri);
  return new vscode.Location(uri, lspRangeToVs(location.range));
}

function lspCompletionToVs(item) {
  const label = item.label || '';
  const completion = new vscode.CompletionItem(label, item.kind);
  if (item.detail) {
    completion.detail = item.detail;
  }
  return completion;
}

function lspDiagnosticToVs(diag) {
  const range = diag.range ? lspRangeToVs(diag.range) : new vscode.Range(0, 0, 0, 0);
  const message = diag.message || 'Unknown diagnostic';
  const severity = diag.severity === 2 ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Error;
  const diagnostic = new vscode.Diagnostic(range, message, severity);
  if (diag.code) {
    diagnostic.code = diag.code;
  }
  return diagnostic;
}

function extractHoverText(contents) {
  if (typeof contents === 'string') {
    return contents;
  }
  if (Array.isArray(contents)) {
    return contents.map(extractHoverText).filter((value) => value).join('\n');
  }
  if (contents && typeof contents.value === 'string') {
    return contents.value;
  }
  return null;
}

module.exports = {
  LspClient
};
