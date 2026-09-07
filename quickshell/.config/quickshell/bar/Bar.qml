// qmllint disable uncreatable-type
import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "widgets" as Widgets
import "../notifications" as Notifications

PanelWindow {
    id: root

    required property var metrics
    required property var config
    property var notifications: null
    implicitHeight: config.barHeight
    color: config.barBackgroundColor
    readonly property int rightMargin: systemTray.visible ? config.barSpacing : 0

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
        id: leftModules

        anchors.left: parent.left
        anchors.verticalCenter: parent.verticalCenter
        spacing: root.config.barSpacing

        Widgets.Workspaces {
            Layout.alignment: Qt.AlignVCenter
            config: root.config
        }
    }

    Widgets.WindowTitle {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        width: Math.max(0, Math.min(
            root.config.titleMaximumWidth,
            parent.width - 2 * Math.max(leftModules.width, rightModules.width + root.rightMargin)
        ))
        config: root.config
    }

    RowLayout {
        id: rightModules

        anchors.right: parent.right
        anchors.rightMargin: root.rightMargin
        anchors.verticalCenter: parent.verticalCenter
        spacing: root.config.barSpacing

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

        Widgets.NotificationCenterButton {
            id: notificationButton

            controller: root.notifications
            config: root.config
        }

        Widgets.Clock {
            config: root.config
        }

        Widgets.SystemTray {
            id: systemTray

            config: root.config
        }
    }

    Notifications.NotificationCenter {
        bar: root
        config: root.config
        controller: root.notifications
        open: notificationButton.centerVisible
        onDismissed: notificationButton.centerVisible = false
    }
}
