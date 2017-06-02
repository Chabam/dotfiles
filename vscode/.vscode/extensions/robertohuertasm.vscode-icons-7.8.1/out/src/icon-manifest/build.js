"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const iconGenerator_1 = require("./iconGenerator");
const supportedExtensions_1 = require("./supportedExtensions");
const supportedFolders_1 = require("./supportedFolders");
const utils_1 = require("../utils");
const defaultSchema_1 = require("./defaultSchema");
const settings_1 = require("../settings");
const iconGenerator = new iconGenerator_1.IconGenerator(utils_1.vscode, defaultSchema_1.schema, true);
const json = iconGenerator.generateJson(supportedExtensions_1.extensions, supportedFolders_1.extensions);
iconGenerator.persist(settings_1.extensionSettings.iconJsonFileName, json, true);
//# sourceMappingURL=build.js.map