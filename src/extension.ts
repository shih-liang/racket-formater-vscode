import * as vscode from 'vscode'
import { spawnSync } from 'child_process'

export const command = 'racket'

const getFullRange = (document: vscode.TextDocument) => {
    const firstLine = document.lineAt(0)
    const lastLine = document.lineAt(document.lineCount - 1)
    return new vscode.Range(0, firstLine.range.start.character, document.lineCount - 1, lastLine.range.end.character)
}

const format = (text: string) => {
    const mode = vscode.workspace.getConfiguration().get('racket-formatter.formatingMode')
    const args = []
    if (mode === 'indentation') {
        args.push('indent.rkt')
    }
    else {
        args.push('pretty-print.rkt')
    }
    return spawnSync(command, args, { input: text, encoding: 'utf8' })
}

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.commands.registerCommand('racket-formatter.format-racket', () => {
            const { activeTextEditor } = vscode.window

            if (!activeTextEditor) return

            const { document } = activeTextEditor
            const text = document.getText()
            const { stderr, stdout } = format(text)

            const edit = new vscode.WorkspaceEdit()
            const range = getFullRange(document)
            edit.replace(document.uri, range, stdout)
            console.log(text, stdout)
            return vscode.workspace.applyEdit(edit)
        })
    )

    const formatter = vscode.languages.registerDocumentFormattingEditProvider(
        { scheme: 'file', language: 'racket' },
        {
            provideDocumentFormattingEdits: (
                document: vscode.TextDocument,
                options: vscode.FormattingOptions
            ): vscode.ProviderResult<vscode.TextEdit[]> =>
                new Promise((resolve, reject) => {
                    const text = document.getText()
                    const { stderr, stdout } = format(text)

                    const range = getFullRange(document)
                    return resolve([vscode.TextEdit.replace(range, stdout)])
                })
        }
    )
    context.subscriptions.push(formatter)
}

export function deactivate() { }
