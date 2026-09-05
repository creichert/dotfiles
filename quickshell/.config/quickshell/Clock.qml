import QtQuick
import Quickshell

Item {
    implicitWidth: clockText.implicitWidth + 16
    implicitHeight: 30

    SystemClock {
        id: clock
        precision: SystemClock.Minutes
    }

    Text {
        id: clockText
        anchors.centerIn: parent
        text: Qt.formatDateTime(clock.date, "MM/dd/yyyy HH:mm")
        color: "white"
        font.pixelSize: 14
    }
}
