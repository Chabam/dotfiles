Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const vscode = require("vscode");
const util = require("./common");
var outputChannel;
function getOutputChannel() {
    if (outputChannel == undefined)
        outputChannel = vscode.window.createOutputChannel("C/C++");
    return outputChannel;
}
exports.getOutputChannel = getOutputChannel;
function allowExecution(file) {
    return new Promise((resolve, reject) => {
        if (process.platform != 'win32') {
            util.checkFileExists(file).then((exists) => {
                if (exists) {
                    fs.chmod(file, '755', (err) => {
                        if (err) {
                            reject(err);
                            return;
                        }
                        resolve();
                    });
                }
                else {
                    getOutputChannel().appendLine("");
                    getOutputChannel().appendLine(`Warning: Expected file ${file} is missing.`);
                    resolve();
                }
            });
        }
        else {
            resolve();
        }
    });
}
exports.allowExecution = allowExecution;
//# sourceMappingURL=common_vscode.js.map