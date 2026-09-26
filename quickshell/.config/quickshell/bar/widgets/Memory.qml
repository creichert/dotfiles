import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var config
    required property var metrics
    required property var theme
    implicitWidth: memoryRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Row {
        id: memoryRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: `${root.metrics.memoryPercent}% `
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "memory"
            theme: root.theme
        }
    }
}
