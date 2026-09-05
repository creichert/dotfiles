import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland

PanelWindow {
    id: root

    required property var metrics
    implicitHeight: 30
    color: "#802b303b"

    anchors {
        top: true
        left: true
        right: true
    }

    IdleInhibitor {
        window: root
        enabled: idleButton.inhibited
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: 0
        anchors.rightMargin: 4
        spacing: 4

        Workspaces {
            Layout.alignment: Qt.AlignVCenter
        }

        Item {
            Layout.fillWidth: true
        }

        WindowTitle {
            Layout.fillWidth: true
            Layout.maximumWidth: 900
        }

        Item {
            Layout.fillWidth: true
        }

        IdleInhibitorButton {
            id: idleButton
        }

        Volume {}

        Network {
            metrics: root.metrics
        }

        Cpu {
            metrics: root.metrics
        }

        Memory {
            metrics: root.metrics
        }

        Temperature {
            metrics: root.metrics
        }

        Clock {}
    }
}
