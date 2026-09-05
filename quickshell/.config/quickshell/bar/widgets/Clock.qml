import QtQuick
import Quickshell

Rectangle {
    required property var config

    implicitWidth: clockText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight
    color: config.activeBackgroundColor

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    Text {
        id: clockText
        anchors.centerIn: parent
        text: Qt.formatDateTime(clock.date, parent.config.clockFormat)
        color: parent.config.textColor
        font.family: parent.config.fontFamily
        font.pixelSize: parent.config.fontPixelSize
    }
}
