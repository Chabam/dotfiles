'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const DebugProtocol_type = { get method() { return 'C_CPP/DebugProtocol'; } };
function setupDebugProtocolHandler(client) {
    var consoleChannel = vscode.window.createOutputChannel("C/CPP Debug Protocol");
    client.onNotification(DebugProtocol_type, (output) => {
        var outputEditorExist = vscode.window.visibleTextEditors.some((editor) => {
            return editor.document.languageId == 'Log';
        });
        if (!outputEditorExist) {
            consoleChannel.show();
        }
        consoleChannel.appendLine("");
        consoleChannel.appendLine("************************************************************************************************************************");
        consoleChannel.append(`${output}`);
    });
}
exports.setupDebugProtocolHandler = setupDebugProtocolHandler;
//# sourceMappingURL=C_Cpp_DebugProtocol.js.map