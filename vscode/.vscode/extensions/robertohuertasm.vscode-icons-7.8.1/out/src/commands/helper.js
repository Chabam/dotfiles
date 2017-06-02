"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function isFolders(preset) {
    return preset.toLowerCase().includes('folders');
}
exports.isFolders = isFolders;
function getFunc(preset) {
    switch (preset) {
        case 'hideFolders':
            return (iconsJson) => Object.keys(iconsJson.folderNames).length === 0 &&
                iconsJson.iconDefinitions._folder.iconPath === '';
        case 'foldersAllDefaultIcon':
            return (iconsJson) => Object.keys(iconsJson.folderNames).length === 0 &&
                iconsJson.iconDefinitions._folder.iconPath !== '';
        default:
            throw new Error('Not Implemented');
    }
}
exports.getFunc = getFunc;
function getIconName(preset) {
    switch (preset) {
        case 'angular':
            return 'ng';
        case 'jsOfficial':
            return 'js_official';
        case 'tsOfficial':
            return 'typescript_official';
        case 'jsonOfficial':
            return 'json_official';
        default:
            throw new Error('Not Implemented');
    }
}
exports.getIconName = getIconName;
//# sourceMappingURL=helper.js.map