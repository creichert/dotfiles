import QtQuick

Rectangle {
    id: root

    property bool inhibited: false
    implicitWidth: 32
    implicitHeight: 30
    color: inhibited ? "#ecf0f1" : "transparent"

    Text {
        anchors.centerIn: parent
        text: root.inhibited ? "" : ""
        color: root.inhibited ? "#2d3436" : "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.inhibited = !root.inhibited
    }
}
