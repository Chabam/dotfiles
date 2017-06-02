Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const os = require("os");
class LinuxDistribution {
    constructor(name, version) {
        this.name = name;
        this.version = version;
    }
    static GetDistroInformation() {
        let linuxDistro;
        linuxDistro = LinuxDistribution.getDistroInformationFromFile('/etc/os-release')
            .catch(() => {
            return LinuxDistribution.getDistroInformationFromFile('/usr/lib/os-release');
        }).catch(() => {
            return Promise.resolve(new LinuxDistribution('unknown', 'unknown'));
        });
        return linuxDistro;
    }
    static getDistroInformationFromFile(path) {
        return new Promise((resolve, reject) => {
            fs.readFile(path, 'utf8', (error, data) => {
                if (error) {
                    reject(error);
                    return;
                }
                resolve(LinuxDistribution.getDistroInformation(data));
            });
        });
    }
    static getDistroInformation(data) {
        const idKey = 'ID';
        const versionKey = 'VERSION_ID';
        let distroName = 'unknown';
        let distroVersion = 'unknown';
        let keyValues = data.split(os.EOL);
        for (let i = 0; i < keyValues.length; i++) {
            let keyValue = keyValues[i].split('=');
            if (keyValue.length == 2) {
                if (keyValue[0] === idKey) {
                    distroName = keyValue[1];
                }
                else if (keyValue[0] === versionKey) {
                    distroVersion = keyValue[1];
                }
            }
        }
        return new LinuxDistribution(distroName, distroVersion);
    }
}
exports.LinuxDistribution = LinuxDistribution;
//# sourceMappingURL=linuxDistribution.js.map