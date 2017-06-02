Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const vscode_extension_telemetry_1 = require("vscode-extension-telemetry");
exports.LogTelemetry_type = {
    get method() { return 'C_Cpp/LogTelemetry'; }
};
let telemetryReporter;
function activate(context) {
    try {
        telemetryReporter = createReporter(context);
    }
    catch (e) {
    }
}
exports.activate = activate;
function deactivate() { }
exports.deactivate = deactivate;
function logDebuggerEvent(eventName, properties) {
    const eventNamePrefix = "cppdbg/" + "VS/Diagnostics/Debugger/";
    if (telemetryReporter) {
        telemetryReporter.sendTelemetryEvent(eventNamePrefix + eventName, properties);
    }
}
exports.logDebuggerEvent = logDebuggerEvent;
function logLanguageServerEvent(eventName, properties, metrics) {
    const eventNamePrefix = "C_Cpp/LanguageServer/";
    if (telemetryReporter) {
        telemetryReporter.sendTelemetryEvent(eventNamePrefix + eventName, properties, metrics);
    }
}
exports.logLanguageServerEvent = logLanguageServerEvent;
function createReporter(context) {
    let packageInfo = getPackageInfo(context);
    if (packageInfo && packageInfo.aiKey) {
        return new vscode_extension_telemetry_1.default(packageInfo.name, packageInfo.version, packageInfo.aiKey);
    }
    return null;
}
function getPackageInfo(context) {
    let extensionPackagePath = context.asAbsolutePath("package.json");
    let extensionPackage = JSON.parse(fs.readFileSync(extensionPackagePath, 'utf8'));
    if (extensionPackage) {
        return {
            name: extensionPackage.publisher + "." + extensionPackage.name,
            version: extensionPackage.version,
            aiKey: extensionPackage.contributes.debuggers[0].aiKey
        };
    }
    return null;
}
//# sourceMappingURL=telemetry.js.map