"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
function getConfig() {
    return vscode.workspace.getConfiguration();
}
exports.getConfig = getConfig;
function findFiles(include, exclude, maxResults, token) {
    return vscode.workspace.findFiles(include, exclude, maxResults, token);
}
exports.findFiles = findFiles;
function asRelativePath(pathOrUri) {
    return vscode.workspace.asRelativePath(pathOrUri);
}
exports.asRelativePath = asRelativePath;
//# sourceMappingURL=vscode-extensions.js.map