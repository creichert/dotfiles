import QtQuick

Item {
    id: root

    required property var metrics
    required property var config
    readonly property bool available: metrics.batteryPercent !== null
    readonly property bool critical: available
        && metrics.batteryStatus === "Discharging"
        && metrics.batteryPercent <= config.batteryCriticalThreshold

    visible: config.batteryModuleEnabled
    implicitWidth: batteryText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function icon() {
        if (metrics.batteryStatus === "Charging")
            return ""
        if (metrics.batteryStatus === "Full" || metrics.batteryStatus === "Not charging")
            return ""
        if (metrics.batteryPercent <= 10)
            return ""
        if (metrics.batteryPercent <= 30)
            return ""
        if (metrics.batteryPercent <= 50)
            return ""
        if (metrics.batteryPercent <= 75)
            return ""
        return ""
    }

    Text {
        id: batteryText
        anchors.centerIn: parent
        text: root.available
            ? `${Math.round(root.metrics.batteryPercent)}% ${root.icon()}`
            : "--"
        color: root.critical ? root.config.urgentColor : root.config.textColor
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }
}
