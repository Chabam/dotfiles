"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const scanner_1 = require("./scanner");
const token_1 = require("./token");
function lex(input) {
    // We use a character scanner as state for the lexer.
    var state = new scanner_1.Scanner(input);
    var tokens = [];
    var f = LexerFunctions.lexRange;
    while (f) {
        // Each lexing function returns the next lexing function or null.
        f = f(state, tokens);
    }
    return tokens;
}
exports.lex = lex;
function emitToken(type, state) {
    var content = state.emit();
    return (content.length > 0) ? new token_1.Token(type, content) : null;
}
var LexerFunctions;
(function (LexerFunctions) {
    // Starts lexing a Vim command line and delegates on other lexer functions as needed.
    function lexRange(state, tokens) {
        while (true) {
            if (state.isAtEof) {
                break;
            }
            var c = state.next();
            switch (c) {
                case ",":
                case ";":
                    tokens.push(emitToken(token_1.TokenType.Comma, state));
                    continue;
                case "%":
                    tokens.push(emitToken(token_1.TokenType.Percent, state));
                    continue;
                case "$":
                    tokens.push(emitToken(token_1.TokenType.Dollar, state));
                    continue;
                case ".":
                    tokens.push(emitToken(token_1.TokenType.Dot, state));
                    continue;
                case "/":
                    return lexForwardSearch;
                case "?":
                    return lexReverseSearch;
                case "0":
                case "1":
                case "2":
                case "3":
                case "4":
                case "5":
                case "6":
                case "7":
                case "8":
                case "9":
                    return lexLineRef;
                case "+":
                    tokens.push(emitToken(token_1.TokenType.Plus, state));
                    continue;
                case "-":
                    tokens.push(emitToken(token_1.TokenType.Minus, state));
                    continue;
                case "*":
                    state.emit();
                    tokens.push(new token_1.Token(token_1.TokenType.SelectionFirstLine, '<'));
                    tokens.push(new token_1.Token(token_1.TokenType.Comma, ','));
                    tokens.push(new token_1.Token(token_1.TokenType.SelectionLastLine, '>'));
                    continue;
                case "'":
                    return lexMark;
                default:
                    return lexCommand;
            }
        }
        return null;
    }
    LexerFunctions.lexRange = lexRange;
    function lexMark(state, tokens) {
        // The first token has already been lexed.
        if (state.isAtEof) {
            return null;
        }
        var c = state.next();
        switch (c) {
            case '<':
                tokens.push(emitToken(token_1.TokenType.SelectionFirstLine, state));
                break;
            case '>':
                tokens.push(emitToken(token_1.TokenType.SelectionLastLine, state));
                break;
            default:
                if (/[a-zA-Z]/.test(c)) {
                    state.emit();
                    tokens.push(new token_1.Token(token_1.TokenType.Mark, c));
                }
                else {
                    state.backup();
                }
                break;
        }
        return lexRange;
    }
    function lexLineRef(state, tokens) {
        // The first digit has already been lexed.
        while (true) {
            if (state.isAtEof) {
                tokens.push(emitToken(token_1.TokenType.LineNumber, state));
                return null;
            }
            var c = state.next();
            switch (c) {
                case "0":
                case "1":
                case "2":
                case "3":
                case "4":
                case "5":
                case "6":
                case "7":
                case "8":
                case "9":
                    continue;
                default:
                    state.backup();
                    tokens.push(emitToken(token_1.TokenType.LineNumber, state));
                    return lexRange;
            }
        }
    }
    function lexCommand(state, tokens) {
        // The first character of the command's name has already been lexed.
        while (true) {
            if (state.isAtEof) {
                tokens.push(emitToken(token_1.TokenType.CommandName, state));
                break;
            }
            var c = state.next();
            var lc = c.toLowerCase();
            if (lc >= "a" && lc <= "z") {
                continue;
            }
            else {
                state.backup();
                tokens.push(emitToken(token_1.TokenType.CommandName, state));
                while (!state.isAtEof) {
                    state.next();
                }
                // TODO(guillermooo): We need to parse multiple commands.
                var args = emitToken(token_1.TokenType.CommandArgs, state);
                if (args) {
                    tokens.push(args);
                }
                break;
            }
        }
        return null;
    }
    function lexForwardSearch(state, tokens) {
        // The first slash has already been lexed.
        state.skip("/"); // XXX: really?
        var escaping = false;
        var searchTerm = "";
        while (!state.isAtEof) {
            var c = state.next();
            if (c === "/" && !escaping) {
                break;
            }
            if (c === "\\") {
                escaping = true;
                continue;
            }
            else {
                escaping = false;
            }
            searchTerm += c !== "\\" ? c : "\\\\";
        }
        tokens.push(new token_1.Token(token_1.TokenType.ForwardSearch, searchTerm));
        state.ignore();
        if (!state.isAtEof) {
            state.skip("/");
        }
        return lexRange;
    }
    function lexReverseSearch(state, tokens) {
        // The first question mark has already been lexed.
        state.skip("?"); // XXX: really?
        var escaping = false;
        var searchTerm = "";
        while (!state.isAtEof) {
            var c = state.next();
            if (c === "?" && !escaping) {
                break;
            }
            if (c === "\\") {
                escaping = true;
                continue;
            }
            else {
                escaping = false;
            }
            searchTerm += c !== "\\" ? c : "\\\\";
        }
        tokens.push(new token_1.Token(token_1.TokenType.ReverseSearch, searchTerm));
        state.ignore();
        if (!state.isAtEof) {
            state.skip("?");
        }
        return lexRange;
    }
})(LexerFunctions || (LexerFunctions = {}));
//# sourceMappingURL=lexer.js.map