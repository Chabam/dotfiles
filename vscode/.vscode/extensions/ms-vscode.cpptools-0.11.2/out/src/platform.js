Object.defineProperty(exports, "__esModule", { value: true });
const os = require("os");
const util = require("./common");
const linuxDistribution_1 = require("./linuxDistribution");
class PlatformInformation {
    constructor(platform, architecture, distribution) {
        this.platform = platform;
        this.architecture = architecture;
        this.distribution = distribution;
    }
    static GetPlatformInformation() {
        let platform = os.platform();
        let architecturePromise;
        let distributionPromise = Promise.resolve(null);
        switch (platform) {
            case "win32":
                architecturePromise = PlatformInformation.GetWindowsArchitecture();
                break;
            case "linux":
                architecturePromise = PlatformInformation.GetUnixArchitecture();
                distributionPromise = linuxDistribution_1.LinuxDistribution.GetDistroInformation();
                break;
            case "darwin":
                architecturePromise = PlatformInformation.GetUnixArchitecture();
                break;
        }
        return Promise.all([architecturePromise, distributionPromise])
            .then(([arch, distro]) => {
            return new PlatformInformation(platform, arch, distro);
        });
    }
    static GetUnknownArchitecture() { return "Unknown"; }
    static GetWindowsArchitecture() {
        return util.execChildProcess('wmic os get osarchitecture', util.getExtensionPath())
            .then((architecture) => {
            if (architecture) {
                let archArray = architecture.split(os.EOL);
                if (archArray.length >= 2) {
                    let arch = archArray[1].trim();
                    if (arch.indexOf('64') >= 0) {
                        return "x86_64";
                    }
                    else if (arch.indexOf('32') >= 0) {
                        return "x86";
                    }
                }
            }
            return PlatformInformation.GetUnknownArchitecture();
        }).catch((error) => {
            return PlatformInformation.GetUnknownArchitecture();
        });
    }
    static GetUnixArchitecture() {
        return util.execChildProcess('uname -m', util.getExtensionPath())
            .then((architecture) => {
            if (architecture) {
                return architecture.trim();
            }
            return null;
        });
    }
}
exports.PlatformInformation = PlatformInformation;
//# sourceMappingURL=platform.js.map