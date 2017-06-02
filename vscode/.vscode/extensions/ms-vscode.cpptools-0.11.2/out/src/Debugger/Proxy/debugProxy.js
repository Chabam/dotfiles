Object.defineProperty(exports, "__esModule", { value: true });
const child_process = require("child_process");
const path = require("path");
const os = require("os");
const util = require("../../common");
const debugProtocol_1 = require("./debugProtocol");
function proxy() {
    util.setExtensionPath(path.resolve(__dirname, "../../../.."));
    var debugAdaptersFolder = util.getDebugAdaptersPath();
    util.checkLockFile().
        then(function (fileExists) {
        if (fileExists) {
            switch (os.platform()) {
                case "win32":
                    return startDebugChildProcess(path.resolve(debugAdaptersFolder, "bin", "OpenDebugAD7.exe"), process.argv.slice(2), debugAdaptersFolder);
                case "linux":
                case "darwin":
                    return startDebugChildProcess(path.resolve(debugAdaptersFolder, "OpenDebugAD7"), process.argv.slice(2), debugAdaptersFolder);
            }
        }
        else {
            var payload = debugProtocol_1.serializeProtocolEvent(new debugProtocol_1.InitializationErrorResponse("Unable to start debugging.  Please wait for the C++ Debugger to finish installation."));
            process.stdout.write(payload);
        }
    })
        .catch(function (reason) {
        util.logToFile(`Promise failed: ${reason}`);
    });
}
function startDebugChildProcess(targetProcess, args, workingFolder) {
    var promise = new Promise(function (resolve, reject) {
        const child = child_process.spawn(targetProcess, args, { cwd: workingFolder });
        child.on('close', (code) => {
            if (code !== 0) {
                reject(new Error(code.toString()));
            }
            else {
                resolve();
            }
        });
        start(process.stdin, process.stdout, child);
    });
    return promise;
}
function start(inStream, outStream, child) {
    inStream.setEncoding('utf8');
    child.on('error', (data) => {
        util.logToFile(`Child error: ${data}`);
    });
    process.on('SIGTERM', () => {
        child.kill();
        process.exit(0);
    });
    process.on('SIGHUP', () => {
        child.kill();
        process.exit(0);
    });
    inStream.on('error', (error) => {
        util.logToFile(`Instream error: ${error}`);
    });
    outStream.on('error', (error) => {
        util.logToFile(`Outstream error: ${error}`);
    });
    child.stdout.on('data', (data) => {
        outStream.write(data);
    });
    inStream.on('data', (data) => {
        child.stdin.write(data);
    });
    inStream.resume();
}
proxy();
//# sourceMappingURL=debugProxy.js.map