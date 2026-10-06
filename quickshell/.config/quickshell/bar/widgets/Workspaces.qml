pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell.Hyprland

RowLayout {
    id: workspaceRow

    required property var config
    required property var theme

    spacing: config.barSpacing

    Connections {
        target: Hyprland

        function onRawEvent(event) {
            if (event.name === "activespecialv2")
                Hyprland.refreshMonitors()
        }
    }

    Repeater {
        model: Hyprland.workspaces

        delegate: WorkspaceButton {
            required property var modelData
            workspace: modelData
            config: workspaceRow.config
            theme: workspaceRow.theme
        }
    }

    Repeater {
        model: Hyprland.workspaces

        delegate: WorkspaceButton {
            required property var modelData
            workspace: modelData
            config: workspaceRow.config
            theme: workspaceRow.theme
            showSpecial: true
        }
    }
}
