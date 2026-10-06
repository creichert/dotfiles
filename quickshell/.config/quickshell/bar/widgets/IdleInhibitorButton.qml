import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    property bool inhibited: false
    implicitWidth: config.barIconButtonWidth
    engaged: inhibited

    contentItem: Components.Icon {
        name: root.inhibited ? "eyeOpen" : "eyeClosed"
        theme: root.theme
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.inhibited = !root.inhibited
    }
}
