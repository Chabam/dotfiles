"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const node = require("../commands/file");
const scanner_1 = require("../scanner");
function parseEditFileCommandArgs(args) {
    if (!args) {
        return new node.FileCommand({});
    }
    let scanner = new scanner_1.Scanner(args);
    let name = scanner.nextWord();
    return new node.FileCommand({
        name: name,
        position: node.FilePosition.CurrentWindow
    });
}
exports.parseEditFileCommandArgs = parseEditFileCommandArgs;
function parseEditNewFileInNewWindowCommandArgs(args) {
    return new node.FileCommand({
        position: node.FilePosition.NewWindow
    });
}
exports.parseEditNewFileInNewWindowCommandArgs = parseEditNewFileInNewWindowCommandArgs;
function parseEditFileInNewWindowCommandArgs(args) {
    let name = "";
    if (args) {
        let scanner = new scanner_1.Scanner(args);
        name = scanner.nextWord();
    }
    return new node.FileCommand({
        name: name,
        position: node.FilePosition.NewWindow
    });
}
exports.parseEditFileInNewWindowCommandArgs = parseEditFileInNewWindowCommandArgs;
//# sourceMappingURL=file.js.map