'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const fs = require("fs");
const vscode = require("vscode");
let defaultSettings = `{
    "configurations": [
        {
            "name": "Mac",
            "includePath": [
                "$\{workspaceRoot\}",
                "/usr/include",
                "/usr/local/include"
            ],
            "defines": [],
            "browse": {
                "path": [
                    "/usr/include",
                    "/usr/local/include"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            }
        },
        {
            "name": "Linux",
            "includePath": [
                "$\{workspaceRoot\}",
                "/usr/include",
                "/usr/local/include"
            ],
            "defines": [],
            "browse": {
                "path": [
                    "/usr/include",
                    "/usr/local/include"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            }
        },
        {
            "name": "Win32",
            "includePath": [
                "$\{workspaceRoot\}",
                "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include"
            ],
            "defines": [
                "_DEBUG",
                "UNICODE"
            ],
            "browse": {
                "path": [
                    "C:/Program Files (x86)/Microsoft Visual Studio 14.0/VC/include/*"
                ],
                "limitSymbolsToIncludedHeaders": true,
                "databaseFilename": ""
            }
        }
    ]
}
`;
const ReportStatus_type = {
    get method() { return 'C_Cpp/ReportStatus'; }
};
const QueryDefaultSdks_type = {
    get method() { return 'C_Cpp/queryDefaultSdks'; }
};
const ChangeFolderSettings_type = {
    get method() { return 'C_Cpp/didChangeFolderSettings'; }
};
const ChangeSelectedSetting_type = {
    get method() { return 'C_Cpp/didChangeSelectedSetting'; }
};
const SwitchHeaderSource_type = {
    get method() { return 'C_Cpp/didSwitchHeaderSource'; }
};
const FileCreated_type = {
    get method() { return 'C_Cpp/fileCreated'; }
};
const FileDeleted_type = {
    get method() { return 'C_Cpp/fileDeleted'; }
};
class ConfigurationProperties {
    UpdateStatusBar() {
        if (this.statusBarItem !== undefined) {
            let activeEditor = vscode.window.activeTextEditor;
            if (!activeEditor || (activeEditor.document.languageId != "cpp" && activeEditor.document.languageId != "c")) {
                this.statusBarItem.hide();
                return;
            }
            this.statusBarItem.text = this.parseStatus + this.configurationJson.configurations[this.currentConfigurationIndex].name;
            if (this.parseStatus == "") {
                this.statusBarItem.color = '';
            }
            else {
                this.statusBarItem.color = 'DarkRed';
            }
            this.statusBarItem.command = "C_Cpp.ConfigurationSelect";
            this.statusBarItem.show();
        }
    }
    getConfigIndexForPlatform(config) {
        if (this.configurationJson.configurations.length > 3)
            return this.configurationJson.configurations.length - 1;
        let nodePlatform = process.platform;
        let plat;
        if (nodePlatform == 'linux') {
            plat = "Linux";
        }
        else if (nodePlatform == 'darwin') {
            plat = "Mac";
        }
        else if (nodePlatform == 'win32') {
            plat = "Win32";
        }
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            if (config.configurations[i].name == plat) {
                return i;
            }
        }
        return this.configurationJson.configurations.length - 1;
    }
    includePathConverted() {
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            if (this.configurationJson.configurations[i].browse === undefined || this.configurationJson.configurations[i].browse.path === undefined) {
                return false;
            }
        }
        return true;
    }
    getDefaultIncludePath() {
        let result = this.defaultSdks.slice(0);
        result.splice(0, 0, "$\{workspaceRoot\}");
        return result;
    }
    constructor(context, client) {
        this.languageClient = client;
        this.registeredCommands = [];
        this.registeredCommands.push(vscode.commands.registerCommand('C_Cpp.SwitchHeaderSource', () => {
            this.handleSwitchHeaderSource();
        }));
        this.languageClient.sendRequest(QueryDefaultSdks_type, {}).then((sdks) => {
            this.defaultSdks = sdks;
            if (!this.propertiesFile) {
                this.handleConfigurationChange();
            }
        });
        if (!vscode.workspace.rootPath) {
            this.registeredCommands.push(vscode.commands.registerCommand('C_Cpp.ConfigurationSelect', () => {
                vscode.window.showInformationMessage('Open a folder first to select a configuration');
            }));
            this.registeredCommands.push(vscode.commands.registerCommand('C_Cpp.ConfigurationEdit', () => {
                vscode.window.showInformationMessage('Open a folder first to edit configurations');
            }));
            this.languageClient.sendNotification(ChangeFolderSettings_type, {
                currentConfiguration: -1,
                configurations: []
            });
            return;
        }
        this.parseStatus = "";
        this.configurationFileName = "**/c_cpp_properties.json";
        let configFilePath = path.join(vscode.workspace.rootPath, ".vscode", "c_cpp_properties.json");
        this.quickPickOptions = {};
        this.currentConfigurationIndex = 0;
        this.statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right);
        this.registeredCommands.push(vscode.commands.registerCommand('C_Cpp.ConfigurationSelect', () => {
            this.handleConfigurationSelectCommand();
        }));
        this.registeredCommands.push(vscode.commands.registerCommand('C_Cpp.ConfigurationEdit', () => {
            this.handleConfigurationEditCommand();
        }));
        if (fs.existsSync(configFilePath)) {
            this.propertiesFile = vscode.Uri.file(configFilePath);
            this.parsePropertiesFile();
            this.currentConfigurationIndex = this.getConfigIndexForPlatform(this.configurationJson);
            this.UpdateStatusBar();
            this.updateServerOnFolderSettingsChange();
        }
        else {
            this.handleConfigurationChange();
        }
        this.configFileWatcher = vscode.workspace.createFileSystemWatcher(this.configurationFileName);
        this.configFileWatcher.onDidCreate((uri) => {
            this.propertiesFile = uri;
            this.handleConfigurationChange();
        });
        this.configFileWatcher.onDidDelete(() => {
            this.propertiesFile = null;
            this.handleConfigurationChange();
        });
        this.configFileWatcher.onDidChange(() => {
            this.handleConfigurationChange();
        });
        this.rootPathFileWatcher = vscode.workspace.createFileSystemWatcher(path.join(vscode.workspace.rootPath, "*"), false, true, false);
        this.rootPathFileWatcher.onDidCreate((uri) => {
            this.languageClient.sendNotification(FileCreated_type, { uri: uri.toString() });
        });
        this.rootPathFileWatcher.onDidDelete((uri) => {
            this.languageClient.sendNotification(FileDeleted_type, { uri: uri.toString() });
        });
        vscode.window.onDidChangeActiveTextEditor((e) => {
            this.UpdateStatusBar();
        });
        this.languageClient.onNotification(ReportStatus_type, (notificationBody) => {
            let message = notificationBody.status;
            if (message.endsWith("...")) {
                this.parseStatus = "$(flame)";
            }
            else if (message.endsWith("Ready")) {
                this.parseStatus = "";
            }
            this.UpdateStatusBar();
        });
    }
    resolveVariables(input) {
        let regexp = /\$\{(.*?)\}/g;
        let ret = input.replace(regexp, (match, name) => {
            let newValue = process.env[name];
            return (newValue != null) ? newValue : match;
        });
        regexp = /^\~/g;
        ret = ret.replace(regexp, (match, name) => {
            let newValue = process.env.HOME;
            return (newValue != null) ? newValue : match;
        });
        return ret;
    }
    updateServerOnFolderSettingsChange() {
        let cppSettings = vscode.workspace.getConfiguration("C_Cpp");
        let addWorkspaceRootToIncludePath = cppSettings.get("addWorkspaceRootToIncludePath");
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            if (this.configurationJson.configurations[i].includePath !== undefined) {
                for (let j = 0; j < this.configurationJson.configurations[i].includePath.length; j++) {
                    this.configurationJson.configurations[i].includePath[j] = this.resolveVariables(this.configurationJson.configurations[i].includePath[j]);
                }
            }
            if (this.configurationJson.configurations[i].browse !== undefined && this.configurationJson.configurations[i].browse.path !== undefined) {
                for (let j = 0; j < this.configurationJson.configurations[i].browse.path.length; j++) {
                    this.configurationJson.configurations[i].browse.path[j] = this.resolveVariables(this.configurationJson.configurations[i].browse.path[j]);
                }
                if (addWorkspaceRootToIncludePath)
                    this.configurationJson.configurations[i].browse.path.splice(0, 0, "$\{workspaceRoot\}");
            }
            else if (addWorkspaceRootToIncludePath) {
                if (this.configurationJson.configurations[i].browse === undefined) {
                    this.configurationJson.configurations[i].browse = {};
                }
                this.configurationJson.configurations[i].browse.path = ["$\{workspaceRoot\}"];
            }
        }
        this.languageClient.sendNotification(ChangeFolderSettings_type, {
            currentConfiguration: this.currentConfigurationIndex,
            configurations: this.configurationJson.configurations
        });
    }
    updateServerOnCurrentConfigurationChange() {
        this.languageClient.sendNotification(ChangeSelectedSetting_type, {
            currentConfiguration: this.currentConfigurationIndex
        });
    }
    updateServerOnSwitchHeaderSourceChange(rootPath_, fileName_) {
        if (rootPath_ == undefined)
            rootPath_ = path.dirname(fileName_);
        this.languageClient.sendRequest(SwitchHeaderSource_type, { rootPath: rootPath_, switchHeaderSourceFileName: fileName_, }).then((targetFileName) => {
            vscode.workspace.openTextDocument(targetFileName).then((document) => {
                let foundEditor = false;
                vscode.window.visibleTextEditors.forEach((editor, index, array) => {
                    if (editor.document == document) {
                        if (!foundEditor) {
                            foundEditor = true;
                            vscode.window.showTextDocument(document, editor.viewColumn);
                        }
                    }
                });
                if (!foundEditor) {
                    if (vscode.window.activeTextEditor != undefined) {
                        vscode.window.showTextDocument(document, vscode.window.activeTextEditor.viewColumn);
                    }
                    else {
                        vscode.window.showTextDocument(document);
                    }
                }
            });
        });
    }
    parsePropertiesFile() {
        try {
            let readResults = fs.readFileSync(this.propertiesFile.fsPath, 'utf8');
            if (readResults == "")
                return;
            this.configurationJson = JSON.parse(readResults);
            if (!this.includePathConverted()) {
                for (let i = 0; i < this.configurationJson.configurations.length; i++) {
                    let config = this.configurationJson.configurations[i];
                    if (config.browse === undefined) {
                        config.browse = {};
                    }
                    if (config.browse.path === undefined && (this.defaultSdks !== undefined || config.includePath !== undefined)) {
                        config.browse.path = (config.includePath === undefined) ? this.defaultSdks.slice(0) : config.includePath.slice(0);
                    }
                }
                fs.writeFileSync(this.propertiesFile.fsPath, JSON.stringify(this.configurationJson, null, 4));
            }
        }
        catch (err) {
            vscode.window.showErrorMessage('Failed to parse "' + this.propertiesFile.fsPath + '": ' + err.message);
            throw err;
        }
    }
    handleConfigurationChange() {
        if (this.propertiesFile) {
            this.parsePropertiesFile();
            if (this.configurationJson.configurations.length <= this.currentConfigurationIndex) {
                this.currentConfigurationIndex = 0;
            }
        }
        else {
            this.configurationJson = JSON.parse(defaultSettings);
            this.currentConfigurationIndex = this.getConfigIndexForPlatform(this.configurationJson);
            if (this.defaultSdks !== undefined) {
                this.configurationJson.configurations[this.currentConfigurationIndex].includePath = this.getDefaultIncludePath();
                this.configurationJson.configurations[this.currentConfigurationIndex].browse.path = this.defaultSdks.slice(0);
            }
        }
        this.UpdateStatusBar();
        this.updateServerOnFolderSettingsChange();
    }
    handleConfigurationEditCommand() {
        if (this.propertiesFile && fs.existsSync(this.propertiesFile.fsPath)) {
            vscode.workspace.openTextDocument(this.propertiesFile).then((document) => {
                vscode.window.showTextDocument(document);
            });
        }
        else {
            let dirPath = path.join(vscode.workspace.rootPath, ".vscode");
            fs.mkdir(dirPath, (e) => {
                if (!e || e.code === 'EEXIST') {
                    let dirPathEscaped = dirPath.replace("#", "%23");
                    let fullPathToFile = path.join(dirPathEscaped, "c_cpp_properties.json");
                    let filePath = vscode.Uri.parse("untitled:" + fullPathToFile);
                    vscode.workspace.openTextDocument(filePath).then((document) => {
                        let edit = new vscode.WorkspaceEdit;
                        let settings = JSON.parse(defaultSettings);
                        let index = this.getConfigIndexForPlatform(settings);
                        if (this.defaultSdks !== undefined) {
                            settings.configurations[index].includePath = this.getDefaultIncludePath();
                            settings.configurations[index].browse.path = this.defaultSdks.slice(0);
                        }
                        edit.insert(document.uri, new vscode.Position(0, 0), JSON.stringify(settings, null, 4));
                        vscode.workspace.applyEdit(edit).then((status) => {
                            document.save().then(() => {
                                filePath = vscode.Uri.file(path.join(dirPath, "c_cpp_properties.json"));
                                vscode.workspace.openTextDocument(filePath).then((document) => {
                                    vscode.window.showTextDocument(document);
                                });
                            });
                        });
                    });
                }
            });
        }
    }
    handleConfigurationSelectCommand() {
        this.quickPickOptions.placeHolder = "Select a Configuration";
        let items;
        items = [];
        for (let i = 0; i < this.configurationJson.configurations.length; i++) {
            items.push({ label: this.configurationJson.configurations[i].name, description: "", index: i });
        }
        let result = vscode.window.showQuickPick(items, this.quickPickOptions);
        result.then((selection) => {
            if (!selection) {
                return;
            }
            this.currentConfigurationIndex = selection.index;
            this.UpdateStatusBar();
            this.updateServerOnCurrentConfigurationChange();
        });
    }
    handleSwitchHeaderSource() {
        let activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor || !activeEditor.document) {
            return;
        }
        if (activeEditor.document.languageId != "cpp" && activeEditor.document.languageId != "c") {
            return;
        }
        this.updateServerOnSwitchHeaderSourceChange(vscode.workspace.rootPath, activeEditor.document.fileName);
    }
    dispose() {
        this.configFileWatcher.dispose();
        this.rootPathFileWatcher.dispose();
        this.statusBarItem.dispose();
        for (let i = 0; i < this.registeredCommands.length; i++) {
            this.registeredCommands[i].dispose();
        }
    }
}
function setupConfigurationProperties(context, client) {
    let ret = new ConfigurationProperties(context, client);
    return ret;
}
exports.setupConfigurationProperties = setupConfigurationProperties;
//# sourceMappingURL=C_Cpp_ConfigurationProperties.js.map