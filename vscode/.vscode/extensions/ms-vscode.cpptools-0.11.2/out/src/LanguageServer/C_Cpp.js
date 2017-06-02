'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const vscode = require("vscode");
const fs = require("fs");
const vscode_languageclient_1 = require("vscode-languageclient");
const util = require("../common");
const Telemetry = require("../telemetry");
const C_Cpp_DebugProtocol = require("./C_Cpp_DebugProtocol");
const C_Cpp_ConfigurationProperties = require("./C_Cpp_ConfigurationProperties");
const C_Cpp_Feedback = require("./C_Cpp_Feedback");
const ShutdownRequest_type = {
    get method() { return "shutdown"; }
};
const ExitRequest_type = {
    get method() { return "exit"; }
};
const ActiveDocumentChange_type = {
    get method() { return "cpp/activeDocumentChange"; }
};
const TextEditorSelectionChange_type = {
    get method() { return "cpp/textEditorSelectionChange"; }
};
const FormatCausesNoChanges_type = {
    get method() { return "cpp/formatCausesNoChanges"; }
};
const ReloadWindow_Type = { get method() { return 'cpp_reloadWindow'; } };
const ReportNavigation_type = {
    get method() { return 'C_Cpp/ReportNavigation'; }
};
const RequestNavigationList_type = {
    get method() { return 'C_Cpp/requestNavigationList'; }
};
const GoToDeclaration_type = {
    get method() { return "C_Cpp/GoToDeclaration"; }
};
const statusBarMessageTimeout = 3000;
let activeDocument;
let quickPickNavigationOptions;
let statusBarNavigator;
let currentNavigationIndex;
let currentNavigation;
let registeredCommands;
let languageClient;
let previousCppSettings = {};
const maxSettingsLengthForTelemetry = 50;
let prevCrashFile;
let extensionContext;
function activate(context) {
    extensionContext = context;
    let serverModule = getExtensionFilenamePath();
    let clangformatModule = getClangFormatFilenamePath();
    let serverOptions = {
        run: { command: serverModule },
        debug: { command: serverModule }
    };
    let bugUserSettings = new C_Cpp_Feedback.FeedbackState(context);
    let con = vscode.workspace.getConfiguration("C_Cpp");
    let editor = vscode.workspace.getConfiguration("editor");
    let excludeFiles = vscode.workspace.getConfiguration("files.exclude");
    let excludeSearch = vscode.workspace.getConfiguration("search.exclude");
    let clientOptions = {
        documentSelector: ['cpp', "c"],
        synchronize: {
            configurationSection: ['C_Cpp', 'files', 'search']
        },
        initializationOptions: {
            clang_format_path: con.get("clang_format_path"),
            clang_format_style: con.get("clang_format_style"),
            clang_format_fallackStyle: con.get("clang_format_fallackStyle"),
            clang_format_sortIncludes: con.get("clang_format_sortIncludes"),
            clang_format_formatOnSave: con.get("clang_format_formatOnSave"),
            formatting: con.get("formatting"),
            extension_path: context.extensionPath,
            exclude_files: excludeFiles,
            exclude_search: excludeSearch,
            bug_user_count: bugUserSettings.getBugUserCount(),
            bug_user_count_edit: bugUserSettings.getBugUserEditCount(),
            storage_path: context.storagePath,
            tab_size: editor.get("tabSize"),
            intelliSenseEngine: con.get("intelliSenseEngine"),
            autocomplete: con.get("autocomplete"),
            errorSquiggles: con.get("errorSquiggles")
        }
    };
    languageClient = new vscode_languageclient_1.LanguageClient('C/Cpp Language Server', serverOptions, clientOptions);
    C_Cpp_DebugProtocol.setupDebugProtocolHandler(languageClient);
    C_Cpp_Feedback.setupFeedbackHandler(context, languageClient);
    context.subscriptions.push(C_Cpp_ConfigurationProperties.setupConfigurationProperties(context, languageClient));
    context.subscriptions.push(languageClient.start());
    languageClient.onNotification(ReloadWindow_Type, () => {
        let reload = "Reload";
        vscode.window.showInformationMessage("Reload the workspace for the settings change to take effect.", reload).then(value => {
            if (value === reload) {
                vscode.commands.executeCommand("workbench.action.reloadWindow");
            }
        });
    });
    registeredCommands = [];
    registeredCommands.push(vscode.commands.registerCommand('C_Cpp.UnloadLanguageServer', () => {
        languageClient.sendRequest(ShutdownRequest_type, null).then(() => languageClient.sendNotification(ExitRequest_type));
    }));
    let formattedDocToSave = null;
    vscode.window.onDidChangeActiveTextEditor((editor) => {
        let activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || (activeEditor.document.languageId != "cpp" && activeEditor.document.languageId != "c")) {
            activeDocument = "";
            statusBarNavigator.hide();
            return;
        }
        showStatusBarNavigator();
        activeDocument = editor.document.uri.toString();
        let activeDoc = vscode_languageclient_1.Code2Protocol.asTextDocumentIdentifier(editor.document);
        languageClient.sendNotification(ActiveDocumentChange_type, activeDoc);
        languageClient.sendNotification(TextEditorSelectionChange_type, editor.selection.start);
    });
    vscode.window.onDidChangeTextEditorSelection((event) => {
        if (!event.textEditor || event.textEditor.document.uri != vscode.window.activeTextEditor.document.uri || (event.textEditor.document.languageId != "cpp" && event.textEditor.document.languageId != "c"))
            return;
        if (activeDocument != event.textEditor.document.uri.toString()) {
            activeDocument = event.textEditor.document.uri.toString();
            let activeDoc = vscode_languageclient_1.Code2Protocol.asTextDocumentIdentifier(event.textEditor.document);
            languageClient.sendNotification(ActiveDocumentChange_type, activeDoc);
        }
        languageClient.sendNotification(TextEditorSelectionChange_type, event.selections[0].start);
    });
    vscode.workspace.onDidSaveTextDocument((doc) => {
        if (!vscode.window.activeTextEditor || doc != vscode.window.activeTextEditor.document || (doc.languageId != "cpp" && doc.languageId != "c"))
            return;
        if (formattedDocToSave != null) {
            formattedDocToSave = null;
        }
        else if (vscode.workspace.getConfiguration("C_Cpp").get("clang_format_formatOnSave")) {
            formattedDocToSave = doc;
            vscode.commands.executeCommand("editor.action.format");
        }
    });
    languageClient.onNotification(FormatCausesNoChanges_type, () => {
        if (formattedDocToSave != null)
            formattedDocToSave = null;
    });
    vscode.workspace.onDidChangeTextDocument(() => {
        if (formattedDocToSave != null)
            formattedDocToSave.save();
    });
    if (process.platform == "darwin") {
        prevCrashFile = "";
        let crashFolder = path.resolve(process.env.HOME, "Library/Logs/DiagnosticReports");
        fs.stat(crashFolder, (err, stats) => {
            let crashObject = {};
            if (err) {
                crashObject["fs.stat: err.code"] = err.code;
                Telemetry.logLanguageServerEvent("MacCrash", crashObject, null);
                return;
            }
            fs.watch(crashFolder, (event, filename) => {
                if (event != "rename")
                    return;
                if (filename == prevCrashFile)
                    return;
                prevCrashFile = filename;
                if (!filename.startsWith("Microsoft.VSCode.CPP."))
                    return;
                setTimeout(() => {
                    fs.readFile(path.resolve(crashFolder, filename), 'utf8', (err, data) => {
                        if (err) {
                            fs.readFile(path.resolve(crashFolder, filename), 'utf8', handleCrashFileRead);
                            return;
                        }
                        handleCrashFileRead(err, data);
                    });
                }, 5000);
            });
        });
    }
    if (!extensionContext.globalState.get("CPP.quickSuggestionsCleared", false)) {
        extensionContext.globalState.update("CPP.quickSuggestionsCleared", true);
        let con = vscode.workspace.getConfiguration("");
        let langs = ["[cpp]", "[c]"];
        langs.forEach((lang) => {
            let langConfig = con.get(lang);
            if (langConfig != undefined && langConfig["editor.quickSuggestions"] != undefined) {
                if (Object.keys(langConfig).length > 1) {
                    langConfig["editor.quickSuggestions"] = undefined;
                    con.update(lang, langConfig, true);
                }
                else {
                    con.update(lang, undefined, true);
                }
            }
        });
    }
    let nonDefaultInitialCppSettings = {};
    for (var key in con) {
        if (con.inspect(key) == undefined)
            continue;
        previousCppSettings[key] = con.get(key);
        if (previousCppSettings[key] != con.inspect(key).defaultValue) {
            nonDefaultInitialCppSettings[key] = (key == "clang_format_path") ? "..." : String(con.get(key));
            if (nonDefaultInitialCppSettings[key].length > maxSettingsLengthForTelemetry)
                nonDefaultInitialCppSettings[key] = nonDefaultInitialCppSettings[key].substr(0, maxSettingsLengthForTelemetry) + "...";
        }
    }
    Telemetry.logLanguageServerEvent("NonDefaultInitialCppSettings", nonDefaultInitialCppSettings, null);
    languageClient.onNotification(Telemetry.LogTelemetry_type, (notificationBody) => {
        Telemetry.logLanguageServerEvent(notificationBody.event, notificationBody.properties, notificationBody.metrics);
    });
    currentNavigation = "";
    quickPickNavigationOptions = {};
    currentNavigationIndex = 0;
    statusBarNavigator = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 1000);
    registeredCommands.push(vscode.commands.registerCommand('C_Cpp.Navigate', () => {
        handleNavigation();
    }));
    registeredCommands.push(vscode.commands.registerCommand('C_Cpp.GoToDeclaration', () => {
        languageClient.sendRequest(GoToDeclaration_type, null).then(() => vscode.commands.executeCommand("editor.action.goToDeclaration"));
    }));
    registeredCommands.push(vscode.commands.registerCommand('C_Cpp.PeekDeclaration', () => {
        languageClient.sendRequest(GoToDeclaration_type, null).then(() => vscode.commands.executeCommand("editor.action.previewDeclaration"));
    }));
    languageClient.onNotification(ReportNavigation_type, (notificationBody) => {
        if (notificationBody.navigation.startsWith("<def>;")) {
            addFileAssociations(notificationBody.navigation.substr(6));
            return;
        }
        currentNavigation = notificationBody.navigation;
        let maxLength = vscode.workspace.getConfiguration("C_Cpp").get("navigation.length", 60);
        if (currentNavigation.length > maxLength)
            currentNavigation = currentNavigation.substring(0, maxLength - 3).concat("...");
        UpdateNavigationStatusBar();
    });
    vscode.workspace.onDidChangeConfiguration(() => {
        handleSettingsChange();
    });
}
exports.activate = activate;
function handleCrashFileRead(err, data) {
    let crashObject = {};
    if (err) {
        crashObject["readFile: err.code"] = err.code;
        Telemetry.logLanguageServerEvent("MacCrash", crashObject, null);
        return;
    }
    let startCrash = data.indexOf(" Crashed:");
    if (startCrash < 0)
        startCrash = 0;
    let endCrash = data.indexOf("Thread ", startCrash);
    if (endCrash < startCrash)
        endCrash = data.length - 1;
    data = data.substr(startCrash, endCrash - startCrash);
    if (data.length > 16384)
        data = data.substr(0, 16384) + "...";
    crashObject["CrashingThreadCallStack"] = data;
    Telemetry.logLanguageServerEvent("MacCrash", crashObject, null);
}
function handleSettingsChange() {
    let cppSettings = vscode.workspace.getConfiguration("C_Cpp");
    let cppSettingsChange = {};
    for (var key in cppSettings) {
        if (cppSettings.inspect(key) == undefined)
            continue;
        if (cppSettings.get(key) != previousCppSettings[key]) {
            previousCppSettings[key] = cppSettings.get(key);
            cppSettingsChange[key] = (key == "clang_format_path") ? "..." : String(previousCppSettings[key]);
            if (cppSettingsChange[key].length > maxSettingsLengthForTelemetry)
                cppSettingsChange[key] = cppSettingsChange[key].substr(0, maxSettingsLengthForTelemetry) + "...";
        }
    }
    if (Object.keys(cppSettingsChange).length > 0)
        Telemetry.logLanguageServerEvent("CppSettingsChange", cppSettingsChange, null);
}
function showStatusBarNavigator() {
    statusBarNavigator.text = currentNavigation;
    statusBarNavigator.command = "C_Cpp.Navigate";
    statusBarNavigator.show();
}
function UpdateNavigationStatusBar() {
    let activeEditor = vscode.window.activeTextEditor;
    if (!activeEditor || (activeEditor.document.languageId != "cpp" && activeEditor.document.languageId != "c")) {
        statusBarNavigator.hide();
        return;
    }
    showStatusBarNavigator();
}
function handleNavigation() {
    let activeEditor = vscode.window.activeTextEditor;
    if (!activeEditor)
        return;
    languageClient.sendRequest(RequestNavigationList_type, vscode_languageclient_1.Code2Protocol.asTextDocumentIdentifier(activeEditor.document)).then((navigationList) => {
        quickPickNavigationOptions.placeHolder = "Select where to navigate to";
        let items;
        let navlist = navigationList.split(";");
        items = [];
        for (let i = 0; i < navlist.length - 1; i += 2) {
            items.push({ label: navlist[i], description: "", index: Number(navlist[i + 1]) });
        }
        let result = vscode.window.showQuickPick(items, quickPickNavigationOptions);
        result.then((selection) => {
            if (!selection) {
                return;
            }
            vscode.window.activeTextEditor.revealRange(new vscode.Range(selection.index, 0, selection.index, 0));
            vscode.window.activeTextEditor.selection = new vscode.Selection(new vscode.Position(selection.index, 0), new vscode.Position(selection.index, 0));
            UpdateNavigationStatusBar();
        });
    });
}
function addFileAssociations(fileAssociations) {
    let con = vscode.workspace.getConfiguration("files");
    let assoc = con.get("associations");
    let files = fileAssociations.split(";");
    let foundNewAssociation = false;
    for (let i = 0; i < files.length - 1; ++i) {
        let file = files[i];
        if (!(file in assoc)) {
            assoc[file] = "cpp";
            foundNewAssociation = true;
        }
    }
    if (foundNewAssociation)
        con.update("associations", assoc);
}
function deactivate() {
    for (let i = 0; i < registeredCommands.length; i++) {
        registeredCommands[i].dispose();
    }
    Telemetry.logLanguageServerEvent("LanguageServerShutdown");
}
exports.deactivate = deactivate;
function getExtensionFilenamePath() {
    let extensionProcessName = 'Microsoft.VSCode.CPP.Extension';
    let plat = process.platform;
    if (plat == 'linux') {
        extensionProcessName += '.linux';
    }
    else if (plat == 'darwin') {
        extensionProcessName += '.darwin';
    }
    else if (plat == 'win32') {
        extensionProcessName += '.exe';
    }
    else {
        throw "Invalid Platform";
    }
    return path.resolve(util.getExtensionPath(), "bin", extensionProcessName);
}
function getClangFormatFilenamePath() {
    let clangformatProcessName = "clang-format";
    let plat = process.platform;
    if (plat == 'linux') {
        ;
    }
    else if (plat == 'darwin') {
        clangformatProcessName += '.darwin';
    }
    else if (plat == 'win32') {
        clangformatProcessName += '.exe';
    }
    else {
        throw "Invalid Platform";
    }
    return path.resolve(util.getExtensionPath(), "LLVM", "bin", clangformatProcessName);
}
//# sourceMappingURL=C_Cpp.js.map