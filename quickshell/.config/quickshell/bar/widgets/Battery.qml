import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var metrics
    required property var config
    required property var theme
    readonly property bool available: metrics.batteryPercent !== null
    readonly property bool critical: available
        && metrics.batteryStatus === "Discharging"
        && metrics.batteryPercent <= config.batteryCriticalThreshold

    visible: config.batteryModuleEnabled
    implicitWidth: batteryRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function icon() {
        if (metrics.batteryStatus === "Charging")
            return "batteryCharging"
        if (metrics.batteryStatus === "Full" || metrics.batteryStatus === "Not charging")
            return "batteryPlugged"
        if (metrics.batteryPercent <= 10)
            return "batteryEmpty"
        if (metrics.batteryPercent <= 30)
            return "batteryQuarter"
        if (metrics.batteryPercent <= 50)
            return "batteryHalf"
        if (metrics.batteryPercent <= 75)
            return "batteryThreeQuarters"
        return "batteryFull"
    }

    Row {
        id: batteryRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: root.available ? `${Math.round(root.metrics.batteryPercent)}% ` : "--"
            color: root.critical ? root.theme.urgent : root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            visible: root.available
            name: root.icon()
            theme: root.theme
            color: root.critical ? root.theme.urgent : root.theme.primaryText
        }
    }
}
