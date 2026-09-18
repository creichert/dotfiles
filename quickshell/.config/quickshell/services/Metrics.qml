import QtQml
import Quickshell.Io

QtObject {
    id: root

    required property var config
    property real cpuPercent: 0
    property real memoryPercent: 0
    property real temperatureC: 0
    property string interfaceName: ""
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
        memoryPercent = sample.memoryPercent
        temperatureC = sample.temperatureC
        interfaceName = sample.interfaceName

        if (elapsedSeconds > 0 && sameInterface) {
            receiveBytesPerSecond = Math.max(0, (sample.receiveBytes - previousReceiveBytes) / elapsedSeconds)
            transmitBytesPerSecond = Math.max(0, (sample.transmitBytes - previousTransmitBytes) / elapsedSeconds)
        }

        previousReceiveBytes = sample.receiveBytes
        previousTransmitBytes = sample.transmitBytes
        previousTimestamp = sample.timestamp
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
            if (!running)
                root.restartTimer.restart()
        }
    }
}
