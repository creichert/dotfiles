import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var config
    required property var metrics
    required property var theme
    implicitWidth: cpuRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Row {
        id: cpuRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: `${root.metrics.cpuPercent}% `
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "cpu"
            theme: root.theme
        }
    }
}
