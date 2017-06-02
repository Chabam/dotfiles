Object.defineProperty(exports, "__esModule", { value: true });
const os = require("os");
const path = require("path");
const fs = require("fs");
const vscode = require("vscode");
const configurations_1 = require("./configurations");
class ConfigurationProviderFactory {
    static getConfigurationProvider() {
        switch (os.platform()) {
            case 'win32':
                return new WindowsConfigurationProvider();
            case 'darwin':
                return new OSXConfigurationProvider();
            case 'linux':
                return new LinuxConfigurationProvider();
            default:
                throw new Error("Unexpected OS type");
        }
    }
}
exports.ConfigurationProviderFactory = ConfigurationProviderFactory;
class DefaultConfigurationProvider {
    getConfigurations() {
        let configurationSnippet = [];
        this.configurations.forEach(configuration => {
            configurationSnippet.push(configuration.GetLaunchConfiguration());
            configurationSnippet.push(configuration.GetAttachConfiguration());
        });
        return configurationSnippet;
    }
    getInitialConfigurations(debuggerType) {
        let configurations = this.getConfigurations().filter(snippet => snippet.debuggerType == debuggerType && snippet.isInitialConfiguration)
            .map(snippet => snippet.bodyText).join(',\n');
        return `
{
\t"version": "0.2.0",
\t"configurations": [
\t\t${configurations_1.indentJsonString(configurations, 2)}
\t]
}`.trim();
    }
    getConfigurationSnippets(context) {
        const manifestPath = path.join(context.extensionPath, 'package.json');
        let manifestString = fs.readFileSync(manifestPath, 'utf8');
        let manifestObject = JSON.parse(manifestString);
        if (manifestObject.contributes.debuggers[0] && !manifestObject.contributes.debuggers[0].configurationSnippets) {
            manifestObject.contributes.debuggers[0].configurationSnippets = this.getConfigurations().map(snippet => {
                delete snippet.isInitialConfiguration;
                delete snippet.debuggerType;
                return snippet;
            });
            manifestString = JSON.stringify(manifestObject, null, 2);
            fs.writeFileSync(manifestPath, manifestString);
            let reload = "Reload";
            vscode.window.showInformationMessage("Please reload for the installation changes to take effect.", reload).then(value => {
                if (value === reload) {
                    vscode.commands.executeCommand("workbench.action.reloadWindow");
                }
            });
        }
        return "";
    }
}
class WindowsConfigurationProvider extends DefaultConfigurationProvider {
    constructor() {
        super();
        this.executable = "a.exe";
        this.pipeProgram = "<full path to pipe program such as plink.exe>";
        this.MIMode = 'gdb';
        this.setupCommandsBlock = `"setupCommands": [
    {
        "description": "Enable pretty-printing for gdb",
        "text": "-enable-pretty-printing",
        "ignoreFailures": true
    }
]`;
        this.configurations = [
            new configurations_1.MIConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock),
            new configurations_1.PipeTransportConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock),
            new configurations_1.WindowsConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock),
            new configurations_1.WSLConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock),
        ];
    }
}
class OSXConfigurationProvider extends DefaultConfigurationProvider {
    constructor() {
        super();
        this.MIMode = 'lldb';
        this.executable = "a.out";
        this.pipeProgram = "/usr/bin/ssh";
        this.configurations = [
            new configurations_1.MIConfigurations(this.MIMode, this.executable, this.pipeProgram),
        ];
    }
}
class LinuxConfigurationProvider extends DefaultConfigurationProvider {
    constructor() {
        super();
        this.MIMode = 'gdb';
        this.setupCommandsBlock = `"setupCommands": [
    {
        "description": "Enable pretty-printing for gdb",
        "text": "-enable-pretty-printing",
        "ignoreFailures": true
    }
]`;
        this.executable = "a.out";
        this.pipeProgram = "/usr/bin/ssh";
        this.configurations = [
            new configurations_1.MIConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock),
            new configurations_1.PipeTransportConfigurations(this.MIMode, this.executable, this.pipeProgram, this.setupCommandsBlock)
        ];
    }
}
//# sourceMappingURL=configurationProvider.js.map