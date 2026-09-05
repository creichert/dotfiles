import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "widgets" as Widgets

PanelWindow {
    id: root

    required property var metrics
    required property var config
    implicitHeight: config.barHeight
    color: config.barBackgroundColor

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
        anchors.rightMargin: root.config.barRightMargin
        spacing: root.config.barSpacing

        Widgets.Workspaces {
            Layout.alignment: Qt.AlignVCenter
            config: root.config
        }

        Item {
            Layout.fillWidth: true
        }

        Widgets.WindowTitle {
            Layout.fillWidth: true
            Layout.maximumWidth: root.config.titleMaximumWidth
            config: root.config
        }

        Item {
            Layout.fillWidth: true
        }

        Widgets.IdleInhibitorButton {
            id: idleButton
            config: root.config
        }

        Widgets.Volume {
            config: root.config
        }

        Widgets.Network {
            metrics: root.metrics
            config: root.config
        }

        Widgets.Cpu {
            metrics: root.metrics
            config: root.config
        }

        Widgets.Memory {
            metrics: root.metrics
            config: root.config
        }

        Widgets.Temperature {
            metrics: root.metrics
            config: root.config
        }

        Widgets.Clock {
            config: root.config
        }
    }
}
