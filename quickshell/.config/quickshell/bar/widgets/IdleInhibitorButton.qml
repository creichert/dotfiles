import QtQuick
import "../../components" as Components

Rectangle {
    id: root

    required property var config
    required property var theme
    property bool inhibited: false
    implicitWidth: config.barIconButtonWidth
    implicitHeight: config.barHeight
    color: inhibited ? theme.selectedSurface : "transparent"

    Components.Icon {
        anchors.centerIn: parent
        name: root.inhibited ? "eyeOpen" : "eyeClosed"
        theme: root.theme
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.inhibited = !root.inhibited
    }
}
