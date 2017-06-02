Object.defineProperty(exports, "__esModule", { value: true });
const common_1 = require("../common");
const os = require("os");
class Process {
    constructor(name, pid, commandLine) {
        this.name = name;
        this.pid = pid;
        this.commandLine = commandLine;
    }
    toAttachItem() {
        return {
            label: this.name,
            description: this.pid,
            detail: this.commandLine,
            id: this.pid
        };
    }
}
class NativeAttachItemsProviderFactory {
    static Get() {
        if (os.platform() === 'win32') {
            return new WmicAttachItemsProvider();
        }
        else {
            return new PsAttachItemsProvider();
        }
    }
}
exports.NativeAttachItemsProviderFactory = NativeAttachItemsProviderFactory;
class NativeAttachItemsProvider {
    getAttachItems() {
        return this.getInternalProcessEntries().then(processEntries => {
            processEntries.sort((a, b) => {
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
            });
            let attachItems = processEntries.map(p => p.toAttachItem());
            return attachItems;
        });
    }
}
class PsAttachItemsProvider extends NativeAttachItemsProvider {
    getInternalProcessEntries() {
        let processCmd = '';
        switch (os.platform()) {
            case 'darwin':
                processCmd = PsProcessParser.psDarwinCommand;
                break;
            case 'linux':
                processCmd = PsProcessParser.psLinuxCommand;
                break;
            default:
                return Promise.reject(new Error(`Operating system "${os.platform()}" not support.`));
        }
        return common_1.execChildProcess(processCmd, null).then(processes => {
            return PsProcessParser.ParseProcessFromPs(processes);
        });
    }
}
exports.PsAttachItemsProvider = PsAttachItemsProvider;
class PsProcessParser {
    static get secondColumnCharacters() { return 50; }
    static get commColumnTitle() { return Array(PsProcessParser.secondColumnCharacters).join("a"); }
    static get psLinuxCommand() { return `ps -axww -o pid=,comm=${PsProcessParser.commColumnTitle},args=`; }
    static get psDarwinCommand() { return `ps -axww -o pid=,comm=${PsProcessParser.commColumnTitle},args= -c`; }
    static ParseProcessFromPs(processes) {
        let lines = processes.split(os.EOL);
        return PsProcessParser.ParseProcessFromPsArray(lines);
    }
    static ParseProcessFromPsArray(processArray) {
        let processEntries = [];
        for (let i = 1; i < processArray.length; i++) {
            let line = processArray[i];
            if (!line) {
                continue;
            }
            let processEntry = PsProcessParser.parseLineFromPs(line);
            processEntries.push(processEntry);
        }
        return processEntries;
    }
    static parseLineFromPs(line) {
        const psEntry = new RegExp(`^\\s*([0-9]+)\\s+(.{${PsProcessParser.secondColumnCharacters - 1}})\\s+(.*)$`);
        const matches = psEntry.exec(line);
        if (matches && matches.length === 4) {
            const pid = matches[1].trim();
            const executable = matches[2].trim();
            const cmdline = matches[3].trim();
            return new Process(executable, pid, cmdline);
        }
    }
}
exports.PsProcessParser = PsProcessParser;
class WmicAttachItemsProvider extends NativeAttachItemsProvider {
    getInternalProcessEntries() {
        const wmicCommand = 'wmic process get Name,ProcessId,CommandLine /FORMAT:list';
        return common_1.execChildProcess(wmicCommand, null).then(processes => {
            return WmicProcessParser.ParseProcessFromWmic(processes);
        });
    }
}
exports.WmicAttachItemsProvider = WmicAttachItemsProvider;
class WmicProcessParser {
    static get wmicNameTitle() { return 'Name'; }
    static get wmicCommandLineTitle() { return 'CommandLine'; }
    ;
    static get wmicPidTitle() { return 'ProcessId'; }
    static ParseProcessFromWmic(processes) {
        let lines = processes.split(os.EOL);
        let currentProcess = new Process(null, null, null);
        let processEntries = [];
        for (let i = 0; i < lines.length; i++) {
            let line = lines[i];
            if (!line) {
                continue;
            }
            WmicProcessParser.parseLineFromWmic(line, currentProcess);
            if (line.lastIndexOf(WmicProcessParser.wmicPidTitle, 0) === 0) {
                processEntries.push(currentProcess);
                currentProcess = new Process(null, null, null);
            }
        }
        return processEntries;
    }
    static parseLineFromWmic(line, process) {
        let splitter = line.indexOf('=');
        if (splitter >= 0) {
            let key = line.slice(0, line.indexOf('=')).trim();
            let value = line.slice(line.indexOf('=') + 1).trim();
            if (key === WmicProcessParser.wmicNameTitle) {
                process.name = value;
            }
            else if (key === WmicProcessParser.wmicPidTitle) {
                process.pid = value;
            }
            else if (key === WmicProcessParser.wmicCommandLineTitle) {
                const extendedLengthPath = '\\??\\';
                if (value.lastIndexOf(extendedLengthPath, 0) === 0) {
                    value = value.slice(extendedLengthPath.length);
                }
                process.commandLine = value;
            }
        }
    }
}
exports.WmicProcessParser = WmicProcessParser;
//# sourceMappingURL=nativeAttach.js.map