import QtQuick

Item {
    required property var metrics
    implicitWidth: memoryText.implicitWidth + 16
    implicitHeight: 30

    Text {
        id: memoryText
        anchors.centerIn: parent
        text: `${parent.metrics.memoryPercent}% `
        color: "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }
}
