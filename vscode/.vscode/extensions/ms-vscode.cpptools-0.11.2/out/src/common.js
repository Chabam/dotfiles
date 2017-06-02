Object.defineProperty(exports, "__esModule", { value: true });
const path = require("path");
const fs = require("fs");
const os = require("os");
const child_process = require("child_process");
var extensionPath;
function setExtensionPath(path) {
    extensionPath = path;
}
exports.setExtensionPath = setExtensionPath;
function getExtensionPath() {
    if (!extensionPath) {
        throw new Error("Failed to set extension path");
    }
    return extensionPath;
}
exports.getExtensionPath = getExtensionPath;
function getDebugAdaptersPath() {
    return path.resolve(getExtensionPath(), "debugAdapters");
}
exports.getDebugAdaptersPath = getDebugAdaptersPath;
function checkLockFile() {
    return checkFileExists(getInstallLockPath());
}
exports.checkLockFile = checkLockFile;
function touchLockFile() {
    return new Promise((resolve, reject) => {
        fs.writeFile(getInstallLockPath(), "", (err) => {
            if (err) {
                reject(err);
            }
            resolve();
        });
    });
}
exports.touchLockFile = touchLockFile;
function checkFileExists(filePath) {
    return new Promise((resolve, reject) => {
        fs.stat(filePath, (err, stats) => {
            if (stats && stats.isFile()) {
                resolve(true);
            }
            else {
                resolve(false);
            }
        });
    });
}
exports.checkFileExists = checkFileExists;
function readFileText(filePath, encoding = "utf8") {
    return new Promise((resolve, reject) => {
        fs.readFile(filePath, encoding, (err, data) => {
            if (err) {
                reject(err);
                return;
            }
            resolve(data);
        });
    });
}
exports.readFileText = readFileText;
function writeFileText(filePath, content, encoding = "utf8") {
    return new Promise((resolve, reject) => {
        fs.writeFile(filePath, content, encoding, (err) => {
            if (err) {
                reject(err);
                return;
            }
            resolve();
        });
    });
}
exports.writeFileText = writeFileText;
function getInstallLockPath() {
    return path.resolve(getExtensionPath(), `install.lock`);
}
exports.getInstallLockPath = getInstallLockPath;
function getReadmeMessage() {
    const readmePath = path.resolve(getExtensionPath(), "README.md");
    const readmeMessage = `Please refer to ${readmePath} for troubleshooting information. Issues can be created at https://github.com/Microsoft/vscppsamples/issues`;
    return readmeMessage;
}
exports.getReadmeMessage = getReadmeMessage;
function logToFile(message) {
    var logFolder = path.resolve(getExtensionPath(), "extension.log");
    fs.writeFileSync(logFolder, `${message}${os.EOL}`, { flag: 'a' });
}
exports.logToFile = logToFile;
function execChildProcess(process, workingDirectory, channel) {
    return new Promise((resolve, reject) => {
        child_process.exec(process, { cwd: workingDirectory, maxBuffer: 500 * 1024 }, (error, stdout, stderr) => {
            if (channel) {
                let message = "";
                let err = false;
                if (stdout && stdout.length > 0) {
                    message += stdout;
                }
                if (stderr && stderr.length > 0) {
                    message += stderr;
                    err = true;
                }
                if (error) {
                    message += error.message;
                    err = true;
                }
                if (err) {
                    channel.append(message);
                    channel.show();
                }
            }
            if (error) {
                reject(error);
                return;
            }
            if (stderr && stderr.length > 0) {
                reject(new Error(stderr));
                return;
            }
            resolve(stdout);
        });
    });
}
exports.execChildProcess = execChildProcess;
function spawnChildProcess(process, args, workingDirectory, dataCallback, errorCallback) {
    return new Promise(function (resolve, reject) {
        const child = child_process.spawn(process, args, { cwd: workingDirectory });
        child.stdout.on('data', (data) => {
            dataCallback(`${data}`);
        });
        child.stderr.on('data', (data) => {
            errorCallback(`${data}`);
        });
        child.on('exit', (code) => {
            if (code !== 0) {
                reject(new Error(`${process} exited with error code ${code}`));
            }
            else {
                resolve();
            }
        });
    });
}
exports.spawnChildProcess = spawnChildProcess;
//# sourceMappingURL=common.js.map