pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import "bar"
import "launcher" as LauncherUi
import "services"

ShellRoot {
    Config {
        id: config
    }

    Metrics {
        id: metricsService
        config: config
    }

    Launcher {
        id: launcher
        config: config
    }

    Loader {
        active: launcher.launcherVisible

        sourceComponent: LauncherUi.Launcher {
            config: config
            controller: launcher
        }
    }

    Loader {
        id: notificationLoader

        active: config.notificationServerEnabled

        sourceComponent: Notifications {
            config: config
        }
    }

    Variants {
        model: Quickshell.screens

        Bar {
            required property var modelData
            screen: modelData
            visible: modelData.name === config.primaryMonitor
            config: config
            metrics: metricsService
            notifications: notificationLoader.item
        }
    }
}
