import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var config
    // QQuickItem already owns the resources list property.
    required property var resourcesService
    required property var theme
    implicitWidth: cpuRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Row {
        id: cpuRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: `${root.resourcesService.cpuPercent}% `
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "cpu"
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.resourcesService.togglePanel()
    }
}
