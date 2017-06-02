'use strict';
var vscode = require('vscode');
var logger_1 = require('./utils/logger');
var config_1 = require('./config');
var trailing_spaces_1 = require('./trailing-spaces');
var TrailingSpacesLoader = (function () {
    function TrailingSpacesLoader() {
        this.logger = logger_1.Logger.getInstance();
        this.logger.setPrefix('Trailing Spaces');
        this.config = config_1.Config.getInstance();
        this.config.load();
        this.trailingSpaces = new trailing_spaces_1.TrailingSpaces();
        this.config.onLoad(this.trailingSpaces.loadConfig, this.trailingSpaces);
    }
    TrailingSpacesLoader.prototype.activate = function (subscriptions) {
        subscriptions.push(this);
        vscode.workspace.onDidChangeConfiguration(this.config.load, this.config, subscriptions);
        vscode.commands.registerTextEditorCommand('trailing-spaces.deleteTrailingSpaces', this.trailingSpaces.deleteTrailingSpaces, this.trailingSpaces);
        vscode.commands.registerTextEditorCommand('trailing-spaces.deleteTrailingSpacesModifiedLinesOnly', this.trailingSpaces.deleteTrailingSpacesModifiedLinesOnly, this.trailingSpaces);
        vscode.commands.registerTextEditorCommand('trailing-spaces.highlightTrailingSpaces', this.trailingSpaces.highlightTrailingSpaces, this.trailingSpaces);
        vscode.commands.registerTextEditorCommand('trailing-spaces.deleteInFolder', this.trailingSpaces.deleteInFolder, this.trailingSpaces);
        vscode.commands.registerTextEditorCommand('trailing-spaces.deleteInFolderRecursive', this.trailingSpaces.deleteInFolderRecursive, this.trailingSpaces);
        vscode.commands.registerCommand('trailing-spaces.loadConfig', this.trailingSpaces.loadConfig, this.trailingSpaces);
        this.trailingSpaces.addListeners();
        this.logger.log("Trailing Spaces activated.");
        this.trailingSpaces.initialize();
    };
    TrailingSpacesLoader.prototype.dispose = function () {
    };
    return TrailingSpacesLoader;
}());
Object.defineProperty(exports, "__esModule", { value: true });
exports.default = TrailingSpacesLoader;
//# sourceMappingURL=loader.js.map