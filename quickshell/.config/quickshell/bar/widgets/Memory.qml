import QtQuick

Item {
    required property var metrics
    required property var config
    implicitWidth: memoryText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Text {
        id: memoryText
        anchors.centerIn: parent
        text: `${parent.metrics.memoryPercent}% `
        color: parent.config.textColor
        font.family: parent.config.fontFamily
        font.pixelSize: parent.config.fontPixelSize
    }
}
