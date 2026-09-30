import QtQuick
import Quickshell.Io

Item {
    id: root

    required property var metrics
    required property var config
    property bool panelVisible: false

    readonly property bool sampleAvailable: metrics.sampleAvailable && metrics.samplerRunning
    readonly property bool cpuAvailable: sampleAvailable && metrics.cpuSampleValid
    readonly property real cpuPercent: metrics.cpuPercent
    // Workload levels, not health or hardware-danger thresholds.
    readonly property int cpuModerateThreshold: 25
    readonly property int cpuBusyThreshold: 50
    readonly property int cpuHighThreshold: 80
    readonly property string cpuLevel: !cpuAvailable ? "unknown"
        : cpuPercent < cpuModerateThreshold ? "low"
        : cpuPercent < cpuBusyThreshold ? "moderate"
        : cpuPercent < cpuHighThreshold ? "busy" : "high"
    readonly property string cpuIcon: !cpuAvailable ? "cpu"
        : cpuLevel === "low" ? "cpuLow"
        : cpuLevel === "moderate" ? "cpuModerate"
        : cpuLevel === "busy" ? "cpuBusy" : "cpuHigh"
    readonly property string cpuStatusText: !cpuAvailable ? "Waiting for utilization sample"
        : cpuLevel === "low" ? "Low utilization"
        : cpuLevel === "moderate" ? "Moderate utilization"
        : cpuLevel === "busy" ? "Busy" : "High utilization"

    readonly property bool memoryAvailable: sampleAvailable && metrics.memoryTotalBytes > 0
    readonly property real memoryPercent: metrics.memoryPercent
    readonly property real memoryTotalBytes: metrics.memoryTotalBytes
    readonly property real memoryUsedBytes: Math.max(0, memoryTotalBytes - metrics.memoryAvailableBytes)
    readonly property string memoryCapacityText: memoryAvailable
        ? `${gib(memoryUsedBytes)} / ${gib(memoryTotalBytes)} GiB used` : "Waiting for memory sample"

    readonly property bool temperatureSupported: metrics.temperatureSupported
    readonly property bool temperatureAvailable: sampleAvailable
        && metrics.temperatureC !== null && Number.isFinite(metrics.temperatureC)
    readonly property var temperatureC: metrics.temperatureC
    readonly property bool temperatureCritical: temperatureAvailable
        && temperatureC >= config.temperatureCriticalThreshold
    readonly property string temperatureLevel: !temperatureAvailable ? "unknown"
        : temperatureCritical ? "critical"
        : temperatureC < config.temperatureCoolThreshold ? "cool"
        : temperatureC < config.temperatureWarmThreshold ? "warm" : "hot"
    readonly property string temperatureIcon: !temperatureAvailable ? "temperatureWarm"
        : temperatureLevel === "cool" ? "temperatureCool"
        : temperatureLevel === "warm" ? "temperatureWarm"
        : temperatureLevel === "hot" ? "temperatureHot" : "temperatureCritical"
    readonly property string temperatureStatusText: !temperatureAvailable ? "Sensor unavailable"
        : temperatureCritical ? `At or above ${config.temperatureCriticalThreshold}°C threshold`
        : temperatureLevel === "cool" ? "Cool"
        : temperatureLevel === "warm" ? "Warm" : "Hot"
    readonly property string temperatureSensorText: [metrics.temperatureSensorLabel,
        metrics.temperatureSensorName].filter(value => value.length > 0).join(" · ") || "CPU sensor"
    readonly property string cadenceText: `CPU/RAM ~${config.metricsIntervalSeconds}s`
        + (temperatureSupported ? ` · temperature ~${config.metricsIntervalSeconds * config.temperatureIntervalSamples}s` : "")

    function gib(bytes) {
        return (bytes / 1073741824).toFixed(1)
    }

    function togglePanel() {
        panelVisible = !panelVisible
    }

    function closePanel() {
        panelVisible = false
    }

    IpcHandler {
        target: "resources"

        function togglePanel(): void {
            root.togglePanel()
        }
    }
}
