"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const packageJson = require("../../../package.json");
function manageAutoApplyCustomizations(isNewVersion, userConfig, applyCustomizationCommand) {
    if (!isNewVersion) {
        return;
    }
    const propObj = packageJson.contributes.configuration.properties;
    for (const key in propObj) {
        if (Reflect.has(propObj, key) && key !== 'vsicons.dontShowNewVersionMessage') {
            const defaultValue = propObj[key].default;
            const parts = key.split('.').filter(x => x !== 'vsicons');
            const userValue = parts.reduce((prev, current) => prev[current], userConfig);
            const cond1 = Array.isArray(defaultValue) && Array.isArray(userValue) && userValue.length;
            // this is to equal null == undefined as vscode doesn't respect null defaults
            // tslint:disable-next-line triple-equals
            const cond2 = !Array.isArray(defaultValue) && defaultValue != userValue;
            if (cond1 || cond2) {
                applyCustomizationCommand();
                return;
            }
        }
    }
}
exports.manageAutoApplyCustomizations = manageAutoApplyCustomizations;
//# sourceMappingURL=autoApplyCustomizations.js.map