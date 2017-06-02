"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const open = require("open");
const i18n_1 = require("../i18n");
const vscode_extensions_1 = require("../utils/vscode-extensions");
const models_1 = require("../models");
const constants_1 = require("../constants");
const i18nManager = new i18n_1.LanguageResourceManager(vscode.env.language);
function manageWelcomeMessage(settingsManager) {
    const state = settingsManager.getState();
    if (!state.welcomeShown) {
        showWelcomeMessage(settingsManager);
        return;
    }
    if (settingsManager.isNewVersion() && !vscode_extensions_1.getConfig().vsicons.dontShowNewVersionMessage) {
        showNewVersionMessage(settingsManager);
    }
}
exports.manageWelcomeMessage = manageWelcomeMessage;
function showWelcomeMessage(settingsManager) {
    settingsManager.updateStatus(models_1.ExtensionStatus.notInstalled);
    vscode.window.showInformationMessage(i18nManager.getMessage(models_1.LangResourceKeys.welcomeBegin, ' ', models_1.LangResourceKeys.activationPath, ' ', models_1.LangResourceKeys.welcomeEnd), { title: i18nManager.getMessage(models_1.LangResourceKeys.aboutOfficialApi) }, { title: i18nManager.getMessage(models_1.LangResourceKeys.seeReadme) })
        .then(btn => {
        if (!btn) {
            return;
        }
        if (btn.title === i18nManager.getMessage(models_1.LangResourceKeys.aboutOfficialApi)) {
            open(i18nManager.getMessage(constants_1.constants.urlOfficialApi));
        }
        else if (btn.title === i18nManager.getMessage(models_1.LangResourceKeys.seeReadme)) {
            open(i18nManager.getMessage(constants_1.constants.urlReadme));
        }
    }, reason => {
        // tslint:disable-next-line:no-console
        console.info('Rejected because: ', reason);
        return;
    });
}
function showNewVersionMessage(settingsManager) {
    const vars = settingsManager.getSettings();
    vscode.window.showInformationMessage(`${i18nManager.getMessage(models_1.LangResourceKeys.newVersion)} v.${vars.extensionSettings.version}`, { title: i18nManager.getMessage(models_1.LangResourceKeys.seeReleaseNotes) }, { title: i18nManager.getMessage(models_1.LangResourceKeys.dontShowThis) })
        .then(btn => {
        settingsManager.updateStatus(models_1.ExtensionStatus.disabled);
        if (!btn) {
            return;
        }
        if (btn.title === i18nManager.getMessage(models_1.LangResourceKeys.seeReleaseNotes)) {
            open(i18nManager.getMessage(constants_1.constants.urlReleaseNote));
        }
        else if (btn.title === i18nManager.getMessage(models_1.LangResourceKeys.dontShowThis)) {
            vscode_extensions_1.getConfig().update('vsicons.dontShowNewVersionMessage', true, true);
        }
    }, reason => {
        // tslint:disable-next-line:no-console
        console.info('Rejected because: ', reason);
        return;
    });
}
//# sourceMappingURL=welcome.js.map