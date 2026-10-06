import QtQml
import Quickshell.Io

QtObject {
    id: root

    required property var config
    property real cpuPercent: 0
    property bool sampleAvailable: false
    property bool cpuSampleValid: false
    property real memoryPercent: 0
    property real memoryTotalBytes: 0
    property real memoryAvailableBytes: 0
    property var temperatureC: null
    property bool temperatureSupported: false
    property string temperatureSensorName: ""
    property string temperatureSensorLabel: ""
    readonly property bool samplerRunning: metricsProcess.running
    property string interfaceName: ""
    property string gateway: ""
    property string linkState: "unknown"
    property real receiveBytesPerSecond: 0
    property real transmitBytesPerSecond: 0
    property real previousReceiveBytes: 0
    property real previousTransmitBytes: 0
    property real previousTimestamp: 0

    function update(line) {
        let sample

        try {
            sample = JSON.parse(line)
        } catch (error) {
            console.warn("Ignoring invalid metrics sample:", error)
            return
        }

        const elapsedSeconds = previousTimestamp > 0
            ? (sample.timestamp - previousTimestamp) / 1000
            : 0
        const sameInterface = sample.interfaceName === interfaceName

        cpuPercent = sample.cpuPercent
        cpuSampleValid = sample.cpuSampleValid === true
        memoryPercent = sample.memoryPercent
        memoryTotalBytes = sample.memoryTotalBytes
        memoryAvailableBytes = sample.memoryAvailableBytes
        temperatureC = sample.temperatureC
        temperatureSupported = sample.temperatureSupported === true
        temperatureSensorName = sample.temperatureSensorName || ""
        temperatureSensorLabel = sample.temperatureSensorLabel || ""
        interfaceName = sample.interfaceName
        gateway = sample.gateway || ""
        linkState = sample.linkState || "unknown"

        if (elapsedSeconds > 0 && sameInterface) {
            receiveBytesPerSecond = Math.max(0, (sample.receiveBytes - previousReceiveBytes) / elapsedSeconds)
            transmitBytesPerSecond = Math.max(0, (sample.transmitBytes - previousTransmitBytes) / elapsedSeconds)
        } else {
            receiveBytesPerSecond = 0
            transmitBytesPerSecond = 0
        }

        previousReceiveBytes = sample.receiveBytes
        previousTransmitBytes = sample.transmitBytes
        previousTimestamp = sample.timestamp
        sampleAvailable = true
    }

    property Timer restartTimer: Timer {
        interval: 1000
        repeat: false
        onTriggered: root.metricsProcess.running = true
    }

    property Process metricsProcess: Process {
        running: true
        command: [
            "bash",
            Qt.resolvedUrl("../scripts/metrics.sh").toString().replace("file://", ""),
            root.config.cpuTemperatureHwmonPath,
            root.config.metricsIntervalSeconds.toString(),
            root.config.temperatureIntervalSamples.toString()
        ]
        stdout: SplitParser {
            onRead: data => root.update(data)
        }
        onRunningChanged: {
            if (!running) {
                root.sampleAvailable = false
                root.cpuSampleValid = false
                root.temperatureC = null
                root.restartTimer.restart()
            }
        }
    }
}
