import QtQuick

Item {
    required property var metrics
    required property var config
    implicitWidth: cpuText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Text {
        id: cpuText
        anchors.centerIn: parent
        text: `${parent.metrics.cpuPercent}% `
        color: parent.config.textColor
        font.family: parent.config.fontFamily
        font.pixelSize: parent.config.fontPixelSize
    }
}
