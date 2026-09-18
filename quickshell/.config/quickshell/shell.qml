//@ pragma DefaultEnv QS_NO_RELOAD_POPUP=1

pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import "bar"
import "clipboard" as ClipboardUi
import "launcher" as LauncherUi
import "services"

ShellRoot {
    id: root

    // A single display does not need an explicit primary-monitor override.
    readonly property var primaryScreen: {
        if (Quickshell.screens.length === 1)
            return Quickshell.screens[0]

        for (const screen of Quickshell.screens) {
            if (screen.name === config.primaryMonitor)
                return screen
        }

        return null
    }
    readonly property bool primaryMonitorMissing: Quickshell.screens.length > 1
        && primaryScreen === null

    function warnMissingPrimaryMonitor() {
        if (primaryMonitorMissing) {
            console.warn(`Primary monitor "${config.primaryMonitor}" not found; bar and notifications are disabled`)
        }
    }

    Component.onCompleted: warnMissingPrimaryMonitor()
    onPrimaryMonitorMissingChanged: warnMissingPrimaryMonitor()

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

    ClipboardHistory {
        id: clipboardHistory
        config: config
    }

    Connections {
        target: launcher

        function onLauncherVisibleChanged() {
            if (launcher.launcherVisible)
                clipboardHistory.close()
        }
    }

    Connections {
        target: clipboardHistory

        function onPickerVisibleChanged() {
            if (clipboardHistory.pickerVisible)
                launcher.close()
        }
    }

    Loader {
        active: launcher.launcherVisible

        sourceComponent: LauncherUi.Launcher {
            config: config
            controller: launcher
        }
    }

    Loader {
        active: clipboardHistory.pickerVisible

        sourceComponent: ClipboardUi.ClipboardPicker {
            config: config
            controller: clipboardHistory
        }
    }

    Loader {
        id: notificationLoader

        active: config.notificationServerEnabled

        sourceComponent: Notifications {
            config: config
            primaryScreen: root.primaryScreen
        }
    }

    Variants {
        model: root.primaryScreen ? [root.primaryScreen] : []

        Bar {
            required property var modelData
            screen: modelData
            config: config
            metrics: metricsService
            notifications: notificationLoader.item
        }
    }
}
