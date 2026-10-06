import QtQuick
import Quickshell.Io
import Quickshell.Services.UPower

Item {
    id: root

    required property var config
    property bool panelVisible: false

    // ready means the initial statistics query succeeded, not live daemon health.
    // 0.3.1 exposes neither backend errors nor an AC Online property. In particular,
    // !UPower.onBattery cannot distinguish AC power from an absent battery.
    readonly property var displayDevice: UPower.displayDevice
    readonly property string dataStatus: {
        if (!displayDevice.ready)
            return "not-ready"
        if (displayDevice.isLaptopBattery)
            return displayDevice.isPresent ? "battery" : "no-battery"
        return displayDevice.type === UPowerDeviceType.Unknown
            ? "no-battery" : "unsupported-device"
    }
    readonly property string chargeState: {
        if (dataStatus !== "battery")
            return "unknown"
        switch (displayDevice.state) {
        case UPowerDeviceState.Charging: return "charging"
        case UPowerDeviceState.Discharging: return "discharging"
        case UPowerDeviceState.Empty: return "empty"
        case UPowerDeviceState.FullyCharged: return "full"
        case UPowerDeviceState.PendingCharge: return "pending-charge"
        case UPowerDeviceState.PendingDischarge: return "pending-discharge"
        default: return "unknown"
        }
    }
    // Charge is normalized to 0..1 by Quickshell; health is NOT normalized.
    readonly property var percent: dataStatus === "battery"
        && Number.isFinite(displayDevice.percentage)
        && displayDevice.percentage >= 0 && displayDevice.percentage <= 1
            ? displayDevice.percentage * 100 : null
    readonly property var timeEstimateSeconds: {
        const seconds = chargeState === "charging" ? displayDevice.timeToFull
            : chargeState === "discharging" ? displayDevice.timeToEmpty : 0
        return Number.isFinite(seconds) && seconds > 0 ? seconds : null
    }
    // Direction comes from state, not EnergyRate's sign. Zero can mean unset.
    readonly property var rateWatts: (chargeState === "charging" || chargeState === "discharging")
        && Number.isFinite(displayDevice.changeRate) && Math.abs(displayDevice.changeRate) > 0
            ? Math.abs(displayDevice.changeRate) : null
    readonly property var fullCapacityWh: dataStatus === "battery"
        && Number.isFinite(displayDevice.energyCapacity) && displayDevice.energyCapacity > 0
            ? displayDevice.energyCapacity : null

    // The aggregate does not report health on our laptop. Only attribute physical
    // device health when exactly one present system battery is reported.
    readonly property var physicalBatteries: UPower.devices.values.filter(device =>
        device.isLaptopBattery && device.isPresent)
    readonly property var healthPercent: {
        if (dataStatus !== "battery" || physicalBatteries.length !== 1)
            return null
        const device = physicalBatteries[0]
        return device.healthSupported && Number.isFinite(device.healthPercentage)
            && device.healthPercentage > 0 ? device.healthPercentage : null
    }
    readonly property bool critical: chargeState === "discharging" && percent !== null
        && percent <= config.batteryCriticalThreshold
    readonly property string statusText: {
        switch (dataStatus) {
        case "not-ready": return "UPower data is not ready or unavailable"
        case "no-battery": return "No system battery reported by UPower"
        case "unsupported-device": return "UPower display device is not a system battery"
        }
        switch (chargeState) {
        case "charging": return "Charging"
        case "discharging": return "Discharging"
        case "empty": return "Empty"
        case "full": return "Fully charged"
        case "pending-charge": return "Waiting to charge"
        case "pending-discharge": return "Waiting to discharge"
        default: return "Battery state unknown"
        }
    }
    readonly property string iconName: {
        if (dataStatus === "no-battery")
            return "batteryAbsent"
        if (dataStatus !== "battery" || chargeState === "unknown")
            return "batteryUnknown"
        if (chargeState === "full")
            return "batteryFullyCharged"
        if (chargeState === "pending-charge")
            return "batteryPlugged"
        if (chargeState === "empty")
            return "batteryEmpty"
        if (percent === null)
            return "batteryUnknown"

        // Nearest 10%, reserving the full-capacity glyph for an actual 100%.
        const step = percent === 100 ? 10 : Math.min(9, Math.round(percent / 10))
        if (chargeState === "charging")
            return step === 0 ? "batteryChargingEmpty" : `batteryCharging${step * 10}`
        return step === 0 ? "batteryEmpty" : step === 10 ? "batteryFull" : `batteryCapacity${step * 10}`
    }

    function togglePanel() {
        if (config.batteryModuleEnabled)
            panelVisible = !panelVisible
    }

    function closePanel() {
        panelVisible = false
    }

    Connections {
        target: root.config

        function onBatteryModuleEnabledChanged() {
            if (!root.config.batteryModuleEnabled)
                root.closePanel()
        }
    }

    IpcHandler {
        target: "battery"

        function togglePanel(): void {
            root.togglePanel()
        }
    }
}
