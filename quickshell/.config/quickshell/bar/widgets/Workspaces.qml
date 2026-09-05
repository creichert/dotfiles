pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell.Hyprland

RowLayout {
    id: workspaceRow

    required property var config
    spacing: 0

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
        }
    }

    Repeater {
        model: Hyprland.workspaces

        delegate: WorkspaceButton {
            required property var modelData
            workspace: modelData
            config: workspaceRow.config
            showSpecial: true
        }
    }
}
