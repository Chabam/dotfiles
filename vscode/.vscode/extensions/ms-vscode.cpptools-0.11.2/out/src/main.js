'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
const os = require("os");
const util = require("./common");
const vsutil = require("./common_vscode");
const Telemetry = require("./telemetry");
const C_Cpp = require("./LanguageServer/C_Cpp");
const DebuggerExtension = require("./Debugger/extension");
const platform_1 = require("./platform");
const packageManager_1 = require("./packageManager");
let delayedCommandsToExecute;
let unregisterTempCommands;
function registerTempCommand(command) {
    unregisterTempCommands.push(vscode.commands.registerCommand(command, () => {
        delayedCommandsToExecute.add(command);
    }));
}
function activate(context) {
    Telemetry.activate(context);
    util.setExtensionPath(context.extensionPath);
    delayedCommandsToExecute = new Set();
    unregisterTempCommands = [];
    registerTempCommand("extension.pickNativeProcess");
    registerTempCommand("extension.pickRemoteNativeProcess");
    registerTempCommand("extension.provideInitialConfigurations_cppvsdbg");
    registerTempCommand("extension.provideInitialConfigurations_cppdbg");
    registerTempCommand("C_Cpp.ConfigurationEdit");
    registerTempCommand("C_Cpp.ConfigurationSelect");
    registerTempCommand("C_Cpp.SwitchHeaderSource");
    registerTempCommand("C_Cpp.UnloadLanguageServer");
    registerTempCommand("C_Cpp.Navigate");
    registerTempCommand("C_Cpp.GoToDeclaration");
    registerTempCommand("C_Cpp.PeekDeclaration");
    processRuntimeDependencies(() => {
        unregisterTempCommands.forEach((command) => {
            command.dispose();
        });
        unregisterTempCommands = [];
        DebuggerExtension.activate(context);
        C_Cpp.activate(context);
        delayedCommandsToExecute.forEach((command) => {
            vscode.commands.executeCommand(command);
        });
        delayedCommandsToExecute.clear();
    });
}
exports.activate = activate;
function deactivate() {
    C_Cpp.deactivate();
    DebuggerExtension.deactivate();
    unregisterTempCommands.forEach((command) => {
        command.dispose();
    });
    Telemetry.deactivate();
}
exports.deactivate = deactivate;
function processRuntimeDependencies(activateExtensions) {
    util.checkLockFile().then((lockExists) => {
        if (!lockExists) {
            let channel = vsutil.getOutputChannel();
            channel.show();
            channel.appendLine("Updating C/C++ dependencies...");
            var statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right);
            let errorMessage = '';
            let hasError = false;
            let installationStage = 'getPlatformInfo';
            let platformInfo;
            let packageManager;
            let telemetryProperties = {};
            platform_1.PlatformInformation.GetPlatformInformation()
                .then((info) => {
                platformInfo = info;
                packageManager = new packageManager_1.PackageManager(info, channel, statusItem);
                channel.appendLine("");
                installationStage = "downloadPackages";
                return packageManager.DownloadPackages();
            })
                .then(() => {
                channel.appendLine("");
                installationStage = "installPackages";
                return packageManager.InstallPackages();
            })
                .then(() => {
                installationStage = "makeBinariesExecutable";
                return vsutil.allowExecution(path.resolve(util.getDebugAdaptersPath(), "OpenDebugAD7"));
            })
                .then(() => {
                installationStage = "rewriteManifest";
                return rewriteManifest();
            })
                .then(() => {
                checkDistro(channel, platformInfo);
                installationStage = "touchLockFile";
                return util.touchLockFile();
            })
                .catch((error) => {
                hasError = true;
                telemetryProperties['stage'] = installationStage;
                if (error instanceof packageManager_1.PackageManagerError) {
                    if (error instanceof packageManager_1.PackageManagerWebResponseError) {
                        let webRequestPackageError = error;
                        if (webRequestPackageError.socket) {
                            let address = webRequestPackageError.socket.address();
                            if (address) {
                                telemetryProperties['error.targetIP'] = address.address + ':' + address.port;
                            }
                        }
                    }
                    let packageError = error;
                    telemetryProperties['error.methodName'] = packageError.methodName;
                    telemetryProperties['error.message'] = packageError.message;
                    if (packageError.innerError) {
                        errorMessage = packageError.innerError.toString();
                    }
                    else {
                        errorMessage = packageError.message;
                    }
                    if (packageError.pkg) {
                        telemetryProperties['error.packageName'] = packageError.pkg.description;
                        telemetryProperties['error.packageUrl'] = packageError.pkg.url;
                    }
                    if (packageError.errorCode) {
                        telemetryProperties['error.errorCode'] = packageError.errorCode;
                    }
                }
                else {
                    errorMessage = error.toString();
                }
                channel.appendLine(`Failed at stage: ${installationStage}`);
                channel.appendLine(errorMessage);
            })
                .then(() => {
                channel.appendLine("");
                installationStage = '';
                channel.appendLine("Finished");
                telemetryProperties['success'] = (!hasError).toString();
                if (platformInfo.distribution) {
                    telemetryProperties['linuxDistroName'] = platformInfo.distribution.name;
                    telemetryProperties['linuxDistroVersion'] = platformInfo.distribution.version;
                }
                telemetryProperties['osArchitecture'] = platformInfo.architecture;
                Telemetry.logDebuggerEvent("acquisition", telemetryProperties);
                statusItem.dispose();
                activateExtensions();
            });
        }
        else {
            activateExtensions();
        }
    });
}
function checkDistro(channel, platformInfo) {
    if (platformInfo.platform != 'win32' && platformInfo.platform != 'linux' && platformInfo.platform != 'darwin') {
        channel.appendLine(`Warning: Debugging has not been tested for this platform. ${util.getReadmeMessage()}`);
    }
}
function rewriteManifest() {
    const manifestPath = path.resolve(util.getExtensionPath(), "package.json");
    return util.readFileText(manifestPath)
        .then((manifestString) => {
        let manifestObject = JSON.parse(manifestString);
        manifestObject.activationEvents = [
            "onLanguage:cpp",
            "onLanguage:c",
            "onCommand:extension.pickNativeProcess",
            "onCommand:extension.pickRemoteNativeProcess",
            "onCommand:extension.provideInitialConfigurations_cppvsdbg",
            "onCommand:extension.provideInitialConfigurations_cppdbg",
            "onCommand:C_Cpp.ConfigurationEdit",
            "onCommand:C_Cpp.ConfigurationSelect",
            "onCommand:C_Cpp.SwitchHeaderSource",
            "onCommand:C_Cpp.UnloadLanguageServer",
            "onCommand:C_Cpp.Navigate",
            "onCommand:C_Cpp.GoToDeclaration",
            "onCommand:C_Cpp.PeekDeclaration",
            "workspaceContains:.vscode/c_cpp_properties.json",
            "onDebug:cppdbg",
            "onDebug:cppvsdbg"
        ];
        manifestObject.contributes.debuggers[0].runtime = undefined;
        manifestObject.contributes.debuggers[0].program = './debugAdapters/OpenDebugAD7';
        manifestObject.contributes.debuggers[0].windows = { "program": "./debugAdapters/bin/OpenDebugAD7.exe" };
        if (os.platform() !== 'win32') {
            manifestObject.contributes.debuggers[1].initialConfigurations = "";
        }
        if (manifestPath.includes(".vscode-insiders")) {
            manifestObject.contributes.configuration.properties["C_Cpp.intelliSenseEngine"].default = "Default";
        }
        manifestString = JSON.stringify(manifestObject, null, 2);
        return util.writeFileText(manifestPath, manifestString);
    });
}
//# sourceMappingURL=main.js.map