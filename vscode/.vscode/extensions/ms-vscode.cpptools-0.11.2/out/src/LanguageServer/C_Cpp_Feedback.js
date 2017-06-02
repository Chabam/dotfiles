'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const os = require("os");
const Telemetry = require("../telemetry");
const BugUser_Type = { get method() { return 'cpp_telemetry/bug_user'; } };
class FeedbackState {
    constructor(context) {
        this.context = context;
        var dbg;
        dbg = false;
        if (dbg) {
            this.setBugUser_Aug2016(true);
            this.setBugUserCount(1);
            this.setBugUserEditCount(1);
        }
    }
    getBugUserCount() {
        if (!this.getBugUser_July2016())
            return this.context.globalState.get("CPP.bugUser.count", 1000);
        return this.context.globalState.get("CPP.bugUser.count", 500);
    }
    setBugUserCount(val) {
        return this.context.globalState.update("CPP.bugUser.count", val);
    }
    getBugUserEditCount() {
        if (!this.getBugUser_July2016())
            return this.context.globalState.get("CPP.bugUser.editCount", 10000);
        return this.context.globalState.get("CPP.bugUser.editCount", 5000);
    }
    setBugUserEditCount(val) {
        return this.context.globalState.update("CPP.bugUser.editCount", val);
    }
    getBugUser_July2016() {
        return this.context.globalState.get("CPP.bugUser", true);
    }
    getBugUser_Aug2016() {
        return this.context.globalState.get("CPP.bugUser.Aug2016", true);
    }
    setBugUser_July2016(val) {
        return this.context.globalState.update("CPP.bugUser", val);
    }
    setBugUser_Aug2016(val) {
        return this.context.globalState.update("CPP.bugUser.Aug2016", val);
    }
    setUserResponded(val) {
        return this.context.globalState.update("CPP.bugUser.responded", val);
    }
}
exports.FeedbackState = FeedbackState;
function setupFeedbackHandler(context, client) {
    var settings = new FeedbackState(context);
    if (settings.getBugUser_Aug2016()) {
        client.onNotification(BugUser_Type, (c) => {
            settings.setBugUser_Aug2016(false);
            Telemetry.logLanguageServerEvent("bugUserForFeedback");
            var message;
            var yesButton;
            var dontAskButton;
            var url;
            var number = Math.random();
            if (!settings.getBugUser_July2016()) {
                message = "Thank you! We've improved a little based on your feedback. Would you like to provide additional feedback for the CPP extension?";
            }
            else {
                message = "Would you like to help us improve the CPP extension?";
            }
            url = "https://aka.ms/egv4z1";
            yesButton = "Yes";
            dontAskButton = "Don't Show Again";
            vscode.window.showInformationMessage(message, dontAskButton, yesButton).then((value) => {
                switch (value) {
                    case yesButton:
                        settings.setUserResponded(true);
                        Telemetry.logLanguageServerEvent("bugUserForFeedbackSuccess");
                        var spawn = require('child_process').spawn;
                        var open_command;
                        if (os.platform() == 'win32') {
                            open_command = 'explorer';
                        }
                        else if (os.platform() == 'darwin') {
                            open_command = '/usr/bin/open';
                        }
                        else {
                            open_command = '/usr/bin/xdg-open';
                        }
                        spawn(open_command, [url]);
                        break;
                    case dontAskButton:
                        settings.setUserResponded(false);
                        settings.setBugUser_Aug2016(false);
                        break;
                }
            });
        });
    }
}
exports.setupFeedbackHandler = setupFeedbackHandler;
//# sourceMappingURL=C_Cpp_Feedback.js.map