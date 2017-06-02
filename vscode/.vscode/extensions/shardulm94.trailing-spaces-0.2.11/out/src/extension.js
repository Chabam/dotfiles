'use strict';
var loader_1 = require('./trailing-spaces/loader');
function activate(context) {
    var trailingSpacesLoader = new loader_1.default();
    trailingSpacesLoader.activate(context.subscriptions);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map