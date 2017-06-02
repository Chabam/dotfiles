Object.defineProperty(exports, "__esModule", { value: true });
function serializeProtocolEvent(message) {
    const payload = JSON.stringify(message);
    const finalPayload = `Content-Length: ${payload.length}\r\n\r\n${payload}`;
    return finalPayload;
}
exports.serializeProtocolEvent = serializeProtocolEvent;
class InitializationErrorResponse {
    constructor(message) {
        this.message = message;
        this.request_seq = 1;
        this.seq = 1;
        this.type = "response";
        this.success = false;
        this.command = "initialize";
    }
}
exports.InitializationErrorResponse = InitializationErrorResponse;
//# sourceMappingURL=debugProtocol.js.map