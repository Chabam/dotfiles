"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const node = require("../node");
const token = require("../token");
const textEditor_1 = require("../../textEditor");
const register_1 = require("../../register/register");
const position_1 = require("../../common/motion/position");
class DeleteRangeCommand extends node.CommandBase {
    constructor(args) {
        super();
        this.neovimCapable = true;
        this._name = 'delete';
        this._arguments = args;
    }
    get arguments() {
        return this._arguments;
    }
    deleteRange(start, end, modeHandler) {
        return __awaiter(this, void 0, void 0, function* () {
            start = start.getLineBegin();
            end = end.getLineEnd();
            end = position_1.Position.FromVSCodePosition(end.with(end.line, end.character + 1));
            const isOnLastLine = end.line === textEditor_1.TextEditor.getLineCount() - 1;
            if (end.character === textEditor_1.TextEditor.getLineAt(end).text.length + 1) {
                end = end.getDown(0);
            }
            if (isOnLastLine && start.line !== 0) {
                start = start.getPreviousLineBegin().getLineEnd();
            }
            let text = modeHandler.vimState.editor.document.getText(new vscode.Range(start, end));
            text = text.endsWith("\r\n") ? text.slice(0, -2) : text.slice(0, -1);
            yield textEditor_1.TextEditor.delete(new vscode.Range(start, end));
            let resultPosition = position_1.Position.EarlierOf(start, end);
            if (start.character > textEditor_1.TextEditor.getLineAt(start).text.length) {
                resultPosition = start.getLeft();
            }
            else {
                resultPosition = start;
            }
            resultPosition = resultPosition.getLineBegin();
            modeHandler.vimState.editor.selection = new vscode.Selection(resultPosition, resultPosition);
            return text;
        });
    }
    execute(modeHandler) {
        return __awaiter(this, void 0, void 0, function* () {
            if (!modeHandler.vimState.editor) {
                return;
            }
            let cursorPosition = position_1.Position.FromVSCodePosition(modeHandler.vimState.editor.selection.active);
            let text = yield this.deleteRange(cursorPosition, cursorPosition, modeHandler);
            register_1.Register.putByKey(text, this._arguments.register, register_1.RegisterMode.LineWise);
        });
    }
    executeWithRange(modeHandler, range) {
        return __awaiter(this, void 0, void 0, function* () {
            let start;
            let end;
            if (range.left[0].type === token.TokenType.Percent) {
                start = new vscode.Position(0, 0);
                end = new vscode.Position(textEditor_1.TextEditor.getLineCount() - 1, 0);
            }
            else {
                start = range.lineRefToPosition(modeHandler.vimState.editor, range.left, modeHandler);
                end = range.lineRefToPosition(modeHandler.vimState.editor, range.right, modeHandler);
            }
            let text = yield this.deleteRange(position_1.Position.FromVSCodePosition(start), position_1.Position.FromVSCodePosition(end), modeHandler);
            register_1.Register.putByKey(text, this._arguments.register, register_1.RegisterMode.LineWise);
        });
    }
}
exports.DeleteRangeCommand = DeleteRangeCommand;
//# sourceMappingURL=deleteRange.js.map