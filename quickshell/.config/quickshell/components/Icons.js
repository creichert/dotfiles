.pragma library

const glyphs = {
    browser: "",
    terminal: "",
    code: "",
    music: "",
    window: "",
    workspaceDefault: "",
    warning: "",
    batteryCharging: "",
    batteryPlugged: "",
    batteryEmpty: "",
    batteryQuarter: "",
    batteryHalf: "",
    batteryThreeQuarters: "",
    batteryFull: "",
    brightnessMinimum: "",
    brightnessLow: "",
    brightnessLowerMiddle: "",
    brightnessMiddle: "",
    brightnessUpperMiddle: "",
    brightnessHigh: "",
    brightnessHigher: "",
    brightnessNearMaximum: "",
    brightnessMaximum: "",
    cpu: "",
    memory: "",
    eyeOpen: "",
    eyeClosed: "",
    bell: "",
    bellMuted: "",
    networkConnected: "󰱔",
    networkDisconnected: "⚠",
    upload: "",
    download: "",
    temperatureCool: "",
    temperatureWarm: "",
    temperatureHot: "",
    temperatureCritical: "",
    volumeOff: "",
    volumeLow: "",
    volumeHigh: "",
    volumeMuted: ""
}

function glyph(name) {
    return glyphs[name] || ""
}
