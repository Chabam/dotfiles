Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const common_1 = require("../common");
const nativeAttach_1 = require("./nativeAttach");
class AttachPicker {
    constructor(attachItemsProvider) {
        this.attachItemsProvider = attachItemsProvider;
    }
    ShowAttachEntries() {
        return this.attachItemsProvider.getAttachItems()
            .then(processEntries => {
            let attachPickOptions = {
                matchOnDescription: true,
                matchOnDetail: true,
                placeHolder: "Select the process to attach to"
            };
            return vscode.window.showQuickPick(processEntries, attachPickOptions)
                .then(chosenProcess => {
                return chosenProcess ? chosenProcess.id : Promise.reject(new Error("Process not selected."));
            });
        });
    }
}
exports.AttachPicker = AttachPicker;
class RemoteAttachPicker {
    constructor() {
        this._channel = null;
        this._channel = vscode.window.createOutputChannel('remote-attach');
    }
    ShowAttachEntries(args) {
        this._channel.clear();
        let pipeTransport = args ? args.pipeTransport : null;
        if (pipeTransport === null) {
            return Promise.reject(new Error("Chosen debug configuration does not contain pipeTransport"));
        }
        let pipeProgram = pipeTransport.pipeProgram;
        let pipeArgs = pipeTransport.pipeArgs;
        let argList = RemoteAttachPicker.createArgumentList(pipeArgs);
        let pipeCmd = `"${pipeProgram}" ${argList}`;
        return this.getRemoteOSAndProcesses(pipeCmd)
            .then(processes => {
            let attachPickOptions = {
                matchOnDetail: true,
                matchOnDescription: true,
                placeHolder: "Select the process to attach to"
            };
            return vscode.window.showQuickPick(processes, attachPickOptions)
                .then(item => {
                return item ? item.id : Promise.reject(new Error("Process not selected."));
            });
        });
    }
    getRemoteOSAndProcesses(pipeCmd) {
        const command = `bash -c 'uname && if [ $(uname) == "Linux" ] ; then ${nativeAttach_1.PsProcessParser.psLinuxCommand} ; elif [ $(uname) == "Darwin" ] ; ` +
            `then ${nativeAttach_1.PsProcessParser.psDarwinCommand}; fi'`;
        return common_1.execChildProcess(`${pipeCmd} "${command}"`, null, this._channel).then(output => {
            let lines = output.split(/\r?\n/);
            if (lines.length == 0) {
                return Promise.reject(new Error("Pipe transport failed to get OS and processes."));
            }
            else {
                let remoteOS = lines[0].replace(/[\r\n]+/g, '');
                if (remoteOS != "Linux" && remoteOS != "Darwin") {
                    return Promise.reject(new Error(`Operating system "${remoteOS}" not supported.`));
                }
                if (lines.length == 1) {
                    return Promise.reject(new Error("Transport attach could not obtain processes list."));
                }
                else {
                    let processes = lines.slice(1);
                    return nativeAttach_1.PsProcessParser.ParseProcessFromPsArray(processes)
                        .sort((a, b) => {
                        if (a.name == undefined) {
                            if (b.name == undefined)
                                return 0;
                            return 1;
                        }
                        if (b.name == undefined)
                            return -1;
                        let aLower = a.name.toLowerCase();
                        let bLower = b.name.toLowerCase();
                        if (aLower == bLower)
                            return 0;
                        return aLower < bLower ? -1 : 1;
                    })
                        .map(p => p.toAttachItem());
                }
            }
        });
    }
    static createArgumentList(args) {
        let argsString = "";
        for (let arg of args) {
            if (argsString) {
                argsString += " ";
            }
            argsString += `"${arg}"`;
        }
        return argsString;
    }
}
exports.RemoteAttachPicker = RemoteAttachPicker;
//# sourceMappingURL=attachToProcess.js.map