import QtQuick
import Quickshell

Item {
    required property var config
    required property var theme

    implicitWidth: clockText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    Text {
        id: clockText
        anchors.centerIn: parent
        text: Qt.formatDateTime(clock.date, parent.config.clockFormat)
        color: parent.theme.primaryText
        font.family: parent.theme.fontFamily
        font.pixelSize: parent.theme.fontPixelSize
    }
}
