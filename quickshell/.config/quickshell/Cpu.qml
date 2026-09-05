import QtQuick

Item {
    required property var metrics
    implicitWidth: cpuText.implicitWidth + 16
    implicitHeight: 30

    Text {
        id: cpuText
        anchors.centerIn: parent
        text: `${parent.metrics.cpuPercent}% `
        color: "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }
}
