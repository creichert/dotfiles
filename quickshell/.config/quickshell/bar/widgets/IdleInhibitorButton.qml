import QtQuick

Rectangle {
    id: root

    required property var config
    property bool inhibited: false
    implicitWidth: 32
    implicitHeight: config.barHeight
    color: inhibited ? config.inhibitedBackgroundColor : "transparent"

    Text {
        anchors.centerIn: parent
        text: root.inhibited ? "" : ""
        color: root.inhibited ? root.config.inhibitedTextColor : root.config.textColor
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.inhibited = !root.inhibited
    }
}
