import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    required property var battery
    visible: config.batteryModuleEnabled
    engaged: battery.panelVisible

    contentItem: Components.Icon {
        name: root.battery.iconName
        theme: root.theme
        // Preserve the existing critical foreground, without adding state backgrounds.
        color: root.battery.critical ? root.theme.urgent : root.theme.primaryText
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton
        onClicked: root.battery.togglePanel()
    }
}
