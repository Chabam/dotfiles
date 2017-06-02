'use strict';
var vscode = require('vscode');
var logger_1 = require('./utils/logger');
var config_1 = require('./config');
var jsdiff = require('diff');
var fs = require('fs');
var path = require('path');
var glob = require('glob');
var TrailingSpaces = (function () {
    function TrailingSpaces() {
        this.decorationOptions = {
            borderRadius: "3px",
            borderWidth: "1px",
            borderStyle: "solid",
            backgroundColor: "rgba(255,0,0,0.3)",
            borderColor: "rgba(255,100,100,0.15)"
        };
        this.logger = logger_1.Logger.getInstance();
        this.config = config_1.Config.getInstance();
        this.languagesToIgnore = {};
        this.matchedRegions = {};
        this.onDisk = {};
        this.loadConfig();
        this.decorationType = vscode.window.createTextEditorDecorationType(this.decorationOptions);
    }
    TrailingSpaces.prototype.loadConfig = function (settings) {
        if (settings)
            this.settings = JSON.parse(settings);
        else
            this.settings = {
                includeEmptyLines: this.config.get("includeEmptyLines"),
                highlightCurrentLine: this.config.get("highlightCurrentLine"),
                regexp: this.config.get("regexp"),
                liveMatching: this.config.get("liveMatching"),
                deleteModifiedLinesOnly: this.config.get("deleteModifiedLinesOnly"),
                syntaxIgnore: this.config.get("syntaxIgnore"),
                trimOnSave: this.config.get("trimOnSave"),
                saveAfterTrim: this.config.get("saveAfterTrim")
            };
        this.matchedRegions = {};
        this.refreshLanguagesToIgnore();
    };
    TrailingSpaces.prototype.refreshLanguagesToIgnore = function () {
        var _this = this;
        this.languagesToIgnore = {};
        this.settings.syntaxIgnore.map(function (language) {
            _this.languagesToIgnore[language] = true;
        });
    };
    TrailingSpaces.prototype.addListeners = function () {
        var _this = this;
        vscode.window.onDidChangeActiveTextEditor(function (editor) {
            if (!editor)
                return;
            _this.logger.log("onDidChangeActiveTextEditor event called - " + editor.document.fileName);
            _this.freezeLastVersion(editor.document);
            if (_this.settings.liveMatching)
                return _this.matchTrailingSpaces(editor);
            return;
        });
        vscode.window.onDidChangeTextEditorSelection(function (e) {
            var editor = e.textEditor;
            _this.logger.log("onDidChangeTextEditorSelection event called - " + editor.document.fileName);
            if (_this.settings.liveMatching)
                _this.matchTrailingSpaces(editor);
            return;
        });
        vscode.workspace.onDidChangeTextDocument(function (e) {
            _this.logger.log("onDidChangeTextDocument event called - " + e.document.fileName);
            if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document == e.document)
                if (_this.settings.liveMatching)
                    _this.matchTrailingSpaces(vscode.window.activeTextEditor);
        });
        vscode.workspace.onDidOpenTextDocument(function (document) {
            _this.logger.log("onDidOpenTextDocument event called - " + document.fileName);
            if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document == document)
                if (_this.settings.liveMatching)
                    _this.matchTrailingSpaces(vscode.window.activeTextEditor);
        });
        vscode.workspace.onDidSaveTextDocument(function (document) {
            _this.logger.log("onDidSaveTextDocument event called - " + document.fileName);
            vscode.window.visibleTextEditors.forEach(function (editor) {
                if (document.uri === editor.document.uri)
                    if (_this.settings.trimOnSave) {
                        editor.edit(function (editBuilder) {
                            _this.deleteTrailingSpaces(editor, editBuilder);
                        }).then(function () {
                            editor.document.save().then(function () {
                                _this.freezeLastVersion(editor.document);
                            });
                        });
                    }
                    else {
                        _this.freezeLastVersion(editor.document);
                    }
            });
        });
        vscode.workspace.onDidCloseTextDocument(function (document) {
            _this.logger.log("onDidCloseTextDocument event called - " + document.fileName);
            _this.onDisk[document.uri.toString()] = null;
        });
    };
    TrailingSpaces.prototype.initialize = function () {
        var _this = this;
        if (this.settings.liveMatching) {
            vscode.window.visibleTextEditors.forEach(function (editor) {
                _this.matchTrailingSpaces(editor);
            });
            this.logger.log("All visible text editors highlighted");
        }
        this.refreshLanguagesToIgnore();
    };
    TrailingSpaces.prototype.deleteTrailingSpaces = function (editor, editorEdit) {
        this.deleteTrailingSpacesCore(editor.document, editor.selection, this.settings);
    };
    TrailingSpaces.prototype.deleteTrailingSpacesModifiedLinesOnly = function (editor, editorEdit) {
        var modifiedLinesSettings = Object.assign({}, this.settings);
        modifiedLinesSettings.deleteModifiedLinesOnly = true;
        this.deleteTrailingSpacesCore(editor.document, editor.selection, modifiedLinesSettings);
    };
    TrailingSpaces.prototype.highlightTrailingSpaces = function (editor, editorEdit) {
        this.matchTrailingSpaces(editor);
    };
    TrailingSpaces.prototype.deleteInFolder = function (editor, editorEdit) {
        this.deleteInFolderCore(editor.document, false);
    };
    TrailingSpaces.prototype.deleteInFolderRecursive = function (editor, editorEdit) {
        this.deleteInFolderCore(editor.document, true);
    };
    TrailingSpaces.prototype.deleteTrailingSpacesCore = function (document, selection, settings) {
        var _this = this;
        var workspaceEdit = new vscode.WorkspaceEdit();
        var end = document.validatePosition(selection.end);
        this.deleteTrailingRegions(document, settings, document.lineAt(end), workspaceEdit);
        if (workspaceEdit.size > 0) {
            vscode.workspace.applyEdit(workspaceEdit).then(function () {
                if (_this.settings.saveAfterTrim && !_this.settings.trimOnSave)
                    document.save();
            });
        }
    };
    TrailingSpaces.prototype.deleteInFolderCore = function (document, recursive) {
        var _this = this;
        if (recursive === void 0) { recursive = false; }
        var folderPath = path.dirname(document.uri.fsPath);
        this.logger.log("Deleting trailing spaces in folder (" + (recursive ? "" : "non-") + "recursive) - " + folderPath);
        var workspaceEdit = new vscode.WorkspaceEdit();
        var totalFilesProcessed = 0;
        var totalEdits = 0;
        var ignoredFiles = 0;
        var globsToIgnore = [];
        function getTrueKeys(obj) {
            var trueKeys = [];
            for (var key in obj) {
                if (obj.hasOwnProperty(key) && obj[key] == true) {
                    trueKeys.push(key);
                }
            }
            return trueKeys;
        }
        globsToIgnore = globsToIgnore.concat(getTrueKeys(vscode.workspace.getConfiguration('search').get('exclude')));
        globsToIgnore = globsToIgnore.map(function (g) { return "**/" + g + "/**"; });
        globsToIgnore = globsToIgnore.concat(getTrueKeys(vscode.workspace.getConfiguration('files').get('exclude')));
        var filePaths = glob.sync(folderPath + (recursive ? "/**" : "") + "/*.*", { nodir: true, ignore: globsToIgnore });
        var promises = [];
        filePaths.forEach(function (filePath) {
            var promise = vscode.workspace.openTextDocument(vscode.Uri.file(filePath)).then(function (document) {
                totalFilesProcessed++;
                if (!document)
                    return;
                var edits = 0;
                edits = _this.deleteTrailingRegions(document, _this.settings, null, workspaceEdit);
                if (edits !== undefined)
                    totalEdits += edits;
                else
                    ignoredFiles++;
                vscode.window.setStatusBarMessage("Processing: " + totalFilesProcessed + "/" + filePaths.length + " - " + filePath);
                _this.logger.log("Processing: " + totalFilesProcessed + "/" + filePaths.length + " - " + filePath);
            }, function (reason) {
                _this.logger.log(reason);
            });
            promises.push(promise);
        }, this);
        Promise.all(promises).then(function () {
            if (workspaceEdit.size > 0) {
                vscode.workspace.applyEdit(workspaceEdit).then(function () {
                    vscode.window.setStatusBarMessage("Deleted " + totalEdits + " trailing spaces in " + (totalFilesProcessed - ignoredFiles) + " files. " + ignoredFiles + " files ignored.");
                });
            }
            else {
                vscode.window.setStatusBarMessage("No trailing spaces to delete in " + (totalFilesProcessed - ignoredFiles) + " files. " + ignoredFiles + " files ignored.");
            }
        }, function (reason) {
            _this.logger.log(reason);
        });
    };
    TrailingSpaces.prototype.deleteTrailingRegions = function (document, settings, currentLine, workspaceEdit) {
        var message;
        var edits = 0;
        if (this.ignoreFile(document)) {
            message = "File with langauge '" + document.languageId + "' ignored.";
            edits = undefined;
        }
        else {
            var regions = this.findRegionsToDelete(document, settings, currentLine);
            if (regions) {
                regions.reverse();
                regions.forEach(function (region) {
                    workspaceEdit.delete(document.uri, region);
                });
            }
            if (regions.length > 0) {
                message = "Deleted " + regions.length + " trailing spaces region" + (regions.length > 1 ? "s" : "");
            }
            else {
                message = "No trailing spaces to delete!";
            }
            edits += regions.length;
        }
        this.logger.log(message);
        vscode.window.setStatusBarMessage(message, 3000);
        return edits;
    };
    TrailingSpaces.prototype.matchTrailingSpaces = function (editor) {
        var regions = { offendingLines: [], highlightable: [] };
        if (this.ignoreFile(editor.document)) {
            this.logger.log("File with langauge '" + editor.document.languageId + "' ignored.");
        }
        else {
            var posn = editor.document.validatePosition(editor.selection.end);
            regions = this.findTrailingSpaces(editor.document, this.settings, editor.document.lineAt(posn));
        }
        this.addTrailingSpacesRegions(editor.document, regions);
        this.highlightTrailingSpacesRegions(editor, regions.highlightable);
    };
    TrailingSpaces.prototype.ignoreFile = function (document) {
        var viewSyntax = document.languageId;
        return viewSyntax ? (this.languagesToIgnore[viewSyntax] == true) : false;
    };
    TrailingSpaces.prototype.addTrailingSpacesRegions = function (document, regions) {
        this.matchedRegions[document.uri.toString()] = regions;
    };
    TrailingSpaces.prototype.highlightTrailingSpacesRegions = function (editor, highlightable) {
        editor.setDecorations(this.decorationType, []);
        editor.setDecorations(this.decorationType, highlightable);
    };
    TrailingSpaces.prototype.modifiedLinesAsNumbers = function (oldFile, newFile) {
        var diffs = jsdiff.diffLines(oldFile, newFile);
        var lineNumber = 0;
        var editedLines = [];
        diffs.forEach(function (diff) {
            if (diff.added)
                editedLines.push(lineNumber);
            if (!diff.removed)
                lineNumber += diff.count;
        });
        return editedLines;
    };
    TrailingSpaces.prototype.getModifiedLineNumbers = function (document) {
        var onBuffer = document.getText();
        return this.modifiedLinesAsNumbers(this.onDisk[document.uri.toString()], onBuffer);
    };
    TrailingSpaces.prototype.freezeLastVersion = function (document) {
        if (document.isUntitled)
            return;
        this.onDisk[document.uri.toString()] = fs.readFileSync(document.uri.fsPath, "utf-8");
        this.logger.log("File frozen - " + document.uri.fsPath);
    };
    TrailingSpaces.prototype.findRegionsToDelete = function (document, settings, currentLine) {
        if (currentLine === void 0) { currentLine = null; }
        var regions;
        if (settings.liveMatching && this.matchedRegions[document.uri.toString()])
            regions = this.matchedRegions[document.uri.toString()];
        else
            regions = this.findTrailingSpaces(document, settings, currentLine);
        if (settings.deleteModifiedLinesOnly && !document.isUntitled) {
            var modifiedLines = this.getModifiedLineNumbers(document);
            function onlyThoseWithTrailingSpaces(regions, modifiedLines) {
                return {
                    offendingLines: regions.offendingLines.filter(function (range) {
                        return (modifiedLines.indexOf(range.start.line) >= 0);
                    }),
                    highlightable: []
                };
            }
            regions = onlyThoseWithTrailingSpaces(regions, modifiedLines);
        }
        return regions.offendingLines;
    };
    TrailingSpaces.prototype.findTrailingSpaces = function (document, settings, currentLine) {
        if (currentLine === void 0) { currentLine = null; }
        var regexp = "(" + settings.regexp + ")$";
        var noEmptyLinesRegexp = "\\S" + regexp;
        var offendingRanges = [];
        var offendingRangesRegexp = new RegExp(settings.includeEmptyLines ? regexp : noEmptyLinesRegexp, "gm");
        var documentText = document.getText();
        var match;
        while (match = offendingRangesRegexp.exec(documentText)) {
            var matchRange = new vscode.Range(document.positionAt(match.index + match[0].length - match[1].length), document.positionAt(match.index + match[0].length));
            if (!matchRange.isEmpty)
                offendingRanges.push(matchRange);
        }
        if (!settings.highlightCurrentLine && currentLine) {
            var highlightable = [];
            var lineText = currentLine.text + '\n';
            var currentOffender = offendingRangesRegexp.exec(lineText);
            var currentOffenderRange = (!currentOffender) ? null : (new vscode.Range(new vscode.Position(currentLine.lineNumber, lineText.lastIndexOf(currentOffender[1])), currentLine.range.end));
            var removal = (!currentOffenderRange) ? null : currentLine.range.intersection(currentOffenderRange);
            if (removal) {
                for (var i = 0; i < offendingRanges.length; i++) {
                    if (!offendingRanges[i].contains(currentOffenderRange)) {
                        highlightable.push(offendingRanges[i]);
                    }
                    else {
                        function splitRange(range) {
                            var returnRanges = [];
                            for (var i_1 = range.start.line; i_1 <= range.end.line; i_1++) {
                                returnRanges.push(document.lineAt(i_1).range.intersection(range));
                            }
                            return returnRanges;
                        }
                        var lineNumber = currentLine.lineNumber;
                        var splitRanges = splitRange(offendingRanges[i]);
                        for (var i_2 = 0; i_2 < splitRanges.length; i_2++) {
                            if (splitRanges[i_2].start.line != lineNumber)
                                highlightable.push(splitRanges[i_2]);
                        }
                    }
                }
            }
            else {
                highlightable = offendingRanges;
            }
            return { offendingLines: offendingRanges, highlightable: highlightable };
        }
        else {
            return { offendingLines: offendingRanges, highlightable: offendingRanges };
        }
    };
    return TrailingSpaces;
}());
exports.TrailingSpaces = TrailingSpaces;
//# sourceMappingURL=trailing-spaces.js.map