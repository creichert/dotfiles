pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type
import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "widgets" as Widgets
import "../audio" as AudioUi
import "../network" as NetworkUi
import "../resources" as ResourcesUi
import "../notifications" as Notifications

PanelWindow {
    id: root

    required property var metrics
    required property var audio
    required property var network
    required property var resources
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

    // Center the clock itself; satellite controls must not affect its position.
    Widgets.Clock {
        id: centerClock

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter
        config: root.config
        theme: root.theme
    }

    RowLayout {
        // Add left satellites before idleButton; anchor future right satellites
        // to centerClock.right in a separate row.
        anchors.right: centerClock.left
        anchors.rightMargin: root.config.barSpacing
        anchors.verticalCenter: centerClock.verticalCenter
        spacing: root.config.barSpacing

        Widgets.IdleInhibitorButton {
            id: idleButton
            config: root.config
            theme: root.theme
        }
    }

    RowLayout {
        id: rightModules

        anchors.right: parent.right
        anchors.rightMargin: root.rightMargin
        anchors.verticalCenter: parent.verticalCenter
        spacing: root.config.barSpacing

        Widgets.Audio {
            id: audioButton
            audio: root.audio
            config: root.config
            theme: root.theme
        }

        Widgets.Resources {
            id: resourcesButton
            resourcesService: root.resources
            config: root.config
            theme: root.theme
        }

        Widgets.Network {
            id: networkButton

            network: root.network
            config: root.config
            theme: root.theme
        }

        Widgets.NotificationCenterButton {
            id: notificationButton

            controller: root.notifications
            config: root.config
            theme: root.theme
        }

        Widgets.SystemTray {
            id: systemTray

            config: root.config
            theme: root.theme
        }
    }

    LazyLoader {
        active: notificationButton.centerVisible

        Notifications.NotificationCenter {
            trigger: notificationButton
            config: root.config
            theme: root.theme
            controller: root.notifications
            open: notificationButton.centerVisible
            onDismissed: root.notifications.notificationCenterVisible = false
        }
    }

    LazyLoader {
        active: root.audio.panelVisible

        AudioUi.AudioPanel {
            trigger: audioButton
            audio: root.audio
            theme: root.theme
        }
    }

    LazyLoader {
        active: root.network.panelVisible

        NetworkUi.NetworkPanel {
            trigger: networkButton
            network: root.network
            theme: root.theme
        }
    }

    LazyLoader {
        active: root.resources.panelVisible

        ResourcesUi.ResourcesPanel {
            trigger: resourcesButton
            resources: root.resources
            theme: root.theme
        }
    }
}
