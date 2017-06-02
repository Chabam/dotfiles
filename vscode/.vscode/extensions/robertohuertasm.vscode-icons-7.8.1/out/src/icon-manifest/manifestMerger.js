"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const _ = require("lodash");
const models = require("../models");
function mergeConfig(customFiles, supportedFiles, customFolders, supportedFolders, iconGenerator) {
    const defaultCustomFiles = customFiles ? customFiles.default : null;
    const defaultCustomFolders = customFolders ? customFolders.default : null;
    const supportedCustomFiles = customFiles ? customFiles.supported : null;
    const supportedCustomFolders = customFolders ? customFolders.supported : null;
    const files = {
        default: mergeDefaultFiles(defaultCustomFiles, supportedFiles.default),
        supported: mergeSupported(supportedCustomFiles, supportedFiles.supported),
    };
    const folders = {
        default: mergeDefaultFolders(defaultCustomFolders, supportedFolders.default),
        supported: mergeSupported(supportedCustomFolders, supportedFolders.supported),
    };
    return iconGenerator.generateJson(files, folders);
}
exports.mergeConfig = mergeConfig;
function mergeDefaultFiles(custom, supported) {
    if (!custom) {
        return supported;
    }
    return {
        file: custom.file || supported.file,
        file_light: custom.file_light || supported.file_light,
    };
}
function mergeDefaultFolders(custom, supported) {
    if (!custom) {
        return supported;
    }
    return {
        folder: custom.folder || supported.folder,
        folder_light: custom.folder_light || supported.folder_light,
    };
}
function mergeSupported(custom, supported) {
    if (!custom || !custom.length) {
        return supported;
    }
    // start the merge operation
    let final = _.cloneDeep(supported);
    custom.forEach(file => {
        const officialFiles = final.filter(x => x.icon === file.icon);
        if (officialFiles.length) {
            // existing icon
            // checking if the icon is disabled
            if (file.disabled != null) {
                officialFiles.forEach(x => x.disabled = file.disabled);
                if (file.disabled) {
                    return;
                }
            }
            file.format = officialFiles[0].format;
        }
        // extends? => copy the icon name to the existing ones.
        // override? => remove overriden extension.
        // check for exentensions in use.
        // we'll add a new node
        if (file.extends) {
            final
                .filter(x => x.icon === file.extends)
                .forEach(x => {
                x.icon = file.icon;
            });
        }
        // remove overrides
        final = final.filter(x => x.icon !== file.overrides);
        // check if file extensions are already in use and remove them
        if (!file.extensions) {
            file.extensions = [];
        }
        file.extensions.forEach(ext => {
            final
                .filter(x => x.extensions.find(y => y === ext))
                .forEach(x => _.remove(x.extensions, ext));
        });
        final.push(file);
    });
    return final;
}
function toggleAngularPreset(disable, files) {
    const icons = files.supported
        .filter(x => x.icon.startsWith('ng_') && (x.icon.endsWith('_js') || x.icon.endsWith('_ts')))
        .map(x => x.icon);
    return togglePreset(disable, icons, files);
}
exports.toggleAngularPreset = toggleAngularPreset;
function toggleOfficialIconsPreset(disable, customFiles, officialIcons, defaultIcons) {
    const temp = togglePreset(disable, officialIcons, customFiles);
    return togglePreset(!disable, defaultIcons, temp);
}
exports.toggleOfficialIconsPreset = toggleOfficialIconsPreset;
function toggleHideFoldersPreset(disable, folders) {
    const folderIcons = folders.supported.filter(x => !x.disabled).map(x => x.icon);
    const collection = togglePreset(disable, folderIcons, folders);
    if (folders.default.folder) {
        collection.default.folder.disabled = disable;
    }
    if (folders.default.folder_light) {
        collection.default.folder_light.disabled = disable;
    }
    return collection;
}
exports.toggleHideFoldersPreset = toggleHideFoldersPreset;
function toggleFoldersAllDefaultIconPreset(disable, folders) {
    const folderIcons = folders.supported.filter(x => !x.disabled).map(x => x.icon);
    const collection = togglePreset(disable, folderIcons, folders);
    if (folders.default.folder) {
        collection.default.folder.disabled = false;
    }
    if (folders.default.folder_light) {
        collection.default.folder_light.disabled = false;
    }
    return collection;
}
exports.toggleFoldersAllDefaultIconPreset = toggleFoldersAllDefaultIconPreset;
// Note: generics and union types don't work very well :(
// that's why we had to use IExtensionCollection<> instead of T
function togglePreset(disable, icons, customItems) {
    const workingCopy = _.cloneDeep(customItems);
    icons.forEach(icon => {
        const existing = workingCopy.supported.filter(x => x.icon === icon);
        if (!existing.length) {
            workingCopy.supported.push({ icon, extensions: [], format: models.FileFormat.svg, disabled: disable });
        }
        else {
            existing.forEach(x => { x.disabled = disable; });
        }
    });
    return workingCopy;
}
//# sourceMappingURL=manifestMerger.js.map