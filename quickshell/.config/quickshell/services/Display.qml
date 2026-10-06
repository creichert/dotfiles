// Quickshell 0.3.1 qmltypes omit QProcess::ExitStatus, as in Network/Clipboard.
// qmllint disable signal-handler-parameters

import QtQuick
import Quickshell.Io

Item {
    id: root

    required property var config
    property bool panelVisible: false
    // Brightness is one Display capability, not aggregate Display availability.
    readonly property bool brightnessAvailable: backlight.device !== ""
        && backlight.status !== "unavailable"
    readonly property var brightnessPercent: backlight.raw === null
        ? null : backlight.raw * 100 / backlight.maximum
    // Presentation-only target. Observed brightnessPercent remains authoritative.
    readonly property var brightnessPendingPercent: {
        const target = backlight.queuedRaw !== null ? backlight.queuedRaw : backlight.requestedRaw
        return target === null ? null : target * 100 / backlight.maximum
    }
    readonly property bool brightnessAdjustable: brightnessAvailable
        && brightnessPercent !== null && backlight.adjustmentSupported
    readonly property string brightnessStatus: backlight.status
    readonly property string brightnessStatusText: {
        if (backlight.status === "discovering" || backlight.status === "loading")
            return "Reading backlight brightness"
        if (backlight.status === "unavailable")
            return "No usable backlight available"
        if (backlight.readError)
            return backlight.readError
        if (backlight.controlError)
            return backlight.controlError
        return backlight.adjustmentSupported ? "Backlight brightness"
            : "Brightness is read-only: brightnessctl is unavailable"
    }
    readonly property string brightnessIconName: iconForPercent(brightnessPercent)

    QtObject {
        id: backlight

        property string device: ""
        property real maximum: 0
        property var raw: null
        property bool adjustmentSupported: false
        property string status: "discovering"
        property string readError: ""
        property string controlError: ""
        // One process and one latest target, not an unbounded command queue.
        property var requestedRaw: null
        property var queuedRaw: null
        // Keep requestedRaw through command completion and FileView confirmation.
        property string confirmation: ""
    }

    function percentFromText(text, maximum) {
        const value = text.trim()
        const raw = Number(value)
        if (!/^[0-9]+$/.test(value) || !Number.isSafeInteger(raw)
                || !Number.isSafeInteger(maximum) || maximum <= 0 || raw > maximum)
            return null
        return raw * 100 / maximum
    }

    function iconForPercent(percent) {
        if (percent === null || !Number.isFinite(percent))
            return "warning"
        const level = Math.min(7, Math.floor(Math.max(0, percent) * 7 / 100) + 1)
        return `brightnessLevel${level}`
    }

    function rawForPercent(percent, maximum) {
        if (!Number.isFinite(percent) || !Number.isSafeInteger(maximum) || maximum <= 0)
            return null
        // Match the existing brightnessctl minimum of one raw unit.
        return Math.max(1, Math.min(maximum, Math.round(Math.max(0, Math.min(100, percent)) * maximum / 100)))
    }

    function discover(text) {
        let result
        try {
            result = JSON.parse(text)
        } catch (error) {
            backlight.readError = "Could not discover backlight brightness"
            backlight.status = "error"
            return
        }
        if (!result.device) {
            backlight.status = "unavailable"
            return
        }
        if (typeof result.device !== "string" || result.device.indexOf("/") !== -1
                || !Number.isSafeInteger(result.maximum) || result.maximum <= 0) {
            backlight.readError = "Invalid backlight discovery result"
            backlight.status = "error"
            return
        }
        backlight.maximum = result.maximum
        backlight.adjustmentSupported = result.adjustmentSupported === true
        backlight.status = "loading"
        backlight.device = result.device
    }

    function updateBrightness(text) {
        const percent = percentFromText(text, backlight.maximum)
        if (percent === null) {
            clearPendingAdjustment()
            backlight.raw = null
            backlight.readError = "Backlight brightness data is invalid"
            backlight.status = "error"
            return
        }
        const raw = Number(text.trim())
        // A file event requests a refresh; only a semantic change updates state.
        if (raw !== backlight.raw)
            backlight.raw = raw
        backlight.readError = ""
        backlight.status = "ready"
        if (backlight.confirmation === "read" && raw !== backlight.requestedRaw) {
            // reload() can share an earlier in-flight FileView read. Once this
            // callback returns, one fresh read confirms the actual post-command
            // value, even if the backend did not apply the requested target.
            backlight.confirmation = "fresh-read"
            Qt.callLater(() => brightnessFile.reload())
        } else if (backlight.confirmation !== "") {
            backlight.confirmation = ""
            backlight.requestedRaw = null
            Qt.callLater(root.startQueuedAdjustment)
        }
    }

    function clearPendingAdjustment() {
        backlight.confirmation = ""
        backlight.requestedRaw = null
        backlight.queuedRaw = null
    }

    function startQueuedAdjustment() {
        if (adjustmentProcess.running || backlight.requestedRaw !== null)
            return
        const target = backlight.queuedRaw
        backlight.queuedRaw = null
        if (!brightnessAdjustable || target === null || target === backlight.raw)
            return
        backlight.controlError = ""
        backlight.requestedRaw = target
        adjustmentProcess.command = ["brightnessctl", "--class=backlight",
            `--device=${backlight.device}`, "--quiet", "set", target.toString()]
        adjustmentProcess.running = true
    }

    function setBrightnessPercent(percent) {
        if (!brightnessAdjustable)
            return false
        const target = rawForPercent(percent, backlight.maximum)
        if (target === null || target === backlight.queuedRaw)
            return false
        if (target === backlight.requestedRaw) {
            backlight.queuedRaw = null
            return false
        }
        if (!adjustmentProcess.running && backlight.requestedRaw === null && target === backlight.raw)
            return false
        backlight.queuedRaw = target
        startQueuedAdjustment()
        return true
    }

    function adjustBrightnessPercent(delta) {
        if (!brightnessAdjustable || !Number.isFinite(delta) || delta === 0)
            return false
        const base = backlight.queuedRaw !== null ? backlight.queuedRaw
            : backlight.requestedRaw !== null ? backlight.requestedRaw : backlight.raw
        return setBrightnessPercent(base * 100 / backlight.maximum + delta)
    }

    function togglePanel() {
        if (config.brightnessModuleEnabled && brightnessAvailable)
            panelVisible = !panelVisible
    }

    function closePanel() {
        panelVisible = false
    }

    onBrightnessAvailableChanged: {
        if (!brightnessAvailable)
            closePanel()
    }

    Connections {
        target: root.config

        function onBrightnessModuleEnabledChanged() {
            if (!root.config.brightnessModuleEnabled)
                root.closePanel()
        }
    }

    Process {
        running: true
        command: ["bash", Qt.resolvedUrl("../scripts/discover-backlight.sh").toString().replace("file://", "")]
        stdout: StdioCollector {
            onStreamFinished: root.discover(text)
        }
        onExited: exitCode => {
            if (exitCode !== 0) {
                backlight.readError = "Could not discover backlight brightness"
                backlight.status = "error"
            }
        }
    }

    FileView {
        id: brightnessFile

        path: backlight.device ? `/sys/class/backlight/${backlight.device}/brightness` : ""
        watchChanges: true
        // Default asynchronous preload. text() is consumed only after loaded,
        // never immediately after reload() (which may still expose old data).
        printErrors: false
        onFileChanged: brightnessFile.reload()
        onLoaded: root.updateBrightness(brightnessFile.text())
        onLoadFailed: error => {
            root.clearPendingAdjustment()
            backlight.raw = null
            backlight.readError = `Could not read backlight brightness: ${FileViewError.toString(error)}`
            backlight.status = error === FileViewError.FileNotFound ? "unavailable" : "error"
        }
    }

    Process {
        id: adjustmentProcess

        onExited: exitCode => {
            if (exitCode === 0) {
                // A matching observed value may already have arrived through
                // fileChanged while brightnessctl was still running.
                if (backlight.raw === backlight.requestedRaw) {
                    backlight.requestedRaw = null
                    Qt.callLater(root.startQueuedAdjustment)
                } else if (backlight.requestedRaw !== null) {
                    backlight.confirmation = "read"
                }
                brightnessFile.reload()
            } else {
                root.clearPendingAdjustment()
                backlight.controlError = `Could not adjust brightness (brightnessctl exit ${exitCode})`
            }
        }
    }

    IpcHandler {
        target: "display"

        function togglePanel(): void {
            root.togglePanel()
        }
    }
}
