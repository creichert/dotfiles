import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var metrics
    required property var config
    required property var theme
    readonly property bool available: metrics.brightnessPercent !== null
    readonly property var icons: [
        "brightnessMinimum", "brightnessLow", "brightnessLowerMiddle",
        "brightnessMiddle", "brightnessUpperMiddle", "brightnessHigh",
        "brightnessHigher", "brightnessNearMaximum", "brightnessMaximum"
    ]

    visible: config.brightnessModuleEnabled
    implicitWidth: brightnessRow.implicitWidth
    implicitHeight: config.barHeight

    function icon() {
        if (!root.available)
            return root.icons[0]

        const index = Math.min(
            root.icons.length - 1,
            Math.floor(root.metrics.brightnessPercent / 100 * root.icons.length)
        )
        return root.icons[index]
    }

    Row {
        id: brightnessRow
        anchors.centerIn: parent
        spacing: root.config.barContentSpacing

        Text {
            text: root.available ? `${Math.round(root.metrics.brightnessPercent)}%` : "--"
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            visible: root.available
            name: root.icon()
            theme: root.theme
        }
    }
}
