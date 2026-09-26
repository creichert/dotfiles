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
    required property var theme
    property var notifications: null
    implicitHeight: config.barHeight
    color: theme.surface
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
            theme: root.theme
        }

        Widgets.Battery {
            Layout.alignment: Qt.AlignVCenter
            metrics: root.metrics
            config: root.config
            theme: root.theme
        }

        Widgets.Brightness {
            Layout.alignment: Qt.AlignVCenter
            metrics: root.metrics
            config: root.config
            theme: root.theme
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
        theme: root.theme
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
            theme: root.theme
        }

        Widgets.Volume {
            config: root.config
            theme: root.theme
        }

        Widgets.Network {
            metrics: root.metrics
            config: root.config
            theme: root.theme
        }

        Widgets.Cpu {
            metrics: root.metrics
            config: root.config
            theme: root.theme
        }

        Widgets.Memory {
            metrics: root.metrics
            config: root.config
            theme: root.theme
        }

        Widgets.Temperature {
            metrics: root.metrics
            config: root.config
            theme: root.theme
        }

        Widgets.NotificationCenterButton {
            id: notificationButton

            controller: root.notifications
            config: root.config
            theme: root.theme
        }

        Widgets.Clock {
            config: root.config
            theme: root.theme
        }

        Widgets.SystemTray {
            id: systemTray

            config: root.config
            theme: root.theme
        }
    }

    Notifications.NotificationCenter {
        trigger: notificationButton
        config: root.config
        theme: root.theme
        controller: root.notifications
        open: notificationButton.centerVisible
        onDismissed: root.notifications.notificationCenterVisible = false
    }
}
