"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const fs = require("fs");
const settings_1 = require("./settings");
const init = require("./init");
const commands = require("./commands");
const vscode_extensions_1 = require("./utils/vscode-extensions");
const utils_1 = require("./utils");
const i18n_1 = require("./i18n");
function initialize(context) {
    const config = vscode_extensions_1.getConfig().vsicons;
    const settingsManager = new settings_1.SettingsManager(vscode);
    commands.registerCommands(context);
    init.manageWelcomeMessage(settingsManager);
    init.manageAutoApplyCustomizations(settingsManager.isNewVersion(), config, commands.applyCustomizationCommand);
    init.detectProject(vscode_extensions_1.findFiles, config)
        .then(results => {
        if (results && results.length && !vscode_extensions_1.asRelativePath(results[0].fsPath).includes('/')) {
            detectAngular(config, results);
        }
    });
    if (settingsManager.isNewVersion()) {
        settingsManager.updateStatus(settingsManager.getState().status);
    }
}
function detectAngular(config, results) {
    let isNgProject;
    for (const result of results) {
        const content = fs.readFileSync(result.fsPath, "utf8");
        const projectJson = utils_1.parseJSON(content);
        isNgProject = projectJson && init.isProject(projectJson, 'ng');
        if (isNgProject) {
            break;
        }
    }
    const i18nManager = new i18n_1.LanguageResourceManager(vscode.env.language);
    const presetValue = vscode_extensions_1.getConfig().inspect(`vsicons.presets.angular`).workspaceValue;
    const result = init.checkForAngularProject(presetValue, init.iconsDisabled('ng'), isNgProject, i18nManager);
    if (!result.apply) {
        return;
    }
    init.applyDetection(i18nManager, result, config.projectDetection.autoReload, commands.applyCustomization, commands.showCustomizationMessage, commands.reload);
}
function activate(context) {
    initialize(context);
    // tslint:disable-next-line no-console
    console.info('vscode-icons is active!');
}
exports.activate = activate;
// this method is called when your vscode is closed
function deactivate() {
    // no code here at the moment
}
exports.deactivate = deactivate;
//# sourceMappingURL=index.js.map