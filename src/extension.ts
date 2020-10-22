import * as vscode from 'vscode'
import { spawnSync } from 'child_process'

export const command = 'racket'
const failed = 0
const succeed = 1

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
    const { stderr, stdout } = spawnSync(command, args, { input: text, encoding: 'utf8' })
    const result = String(stdout)
    const status = result.substr(0, result.indexOf("|"))
    if (status === 'succeed') {
        return result.substr(result.indexOf("|") + 1)
    } else if (status === 'failed') {
        const err = result.substr(result.indexOf("|") + 1)
        vscode.window.showErrorMessage("Racket Formatter: Formatting error, may caused by syntax error in your code file.", err)
        return failed
    } else {
        vscode.window.showErrorMessage("Racket Formatter: Unknown error, if you want help, try to submit a Github issue.")
        return failed
    }
}

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.commands.registerCommand('racket-formatter.format-racket', () => {
            const { activeTextEditor } = vscode.window
            if (!activeTextEditor) return
            const { document } = activeTextEditor
            const text = document.getText()
            const result = format(text)
            if (result === failed) {
                return
            }
            const edit = new vscode.WorkspaceEdit()
            const range = getFullRange(document)
            edit.replace(document.uri, range, result)
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
                    const result = format(text)
                    if (result === failed) {
                        return reject(result)
                    }
                    const range = getFullRange(document)
                    return resolve([vscode.TextEdit.replace(range, result)])
                })
        }
    )
    context.subscriptions.push(formatter)
}

export function deactivate() { }
