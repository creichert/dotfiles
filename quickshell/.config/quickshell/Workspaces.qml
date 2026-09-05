import QtQuick
import QtQuick.Layouts
import Quickshell.Hyprland

RowLayout {
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
        }
    }

    Repeater {
        model: Hyprland.workspaces

        delegate: WorkspaceButton {
            required property var modelData
            workspace: modelData
            showSpecial: true
        }
    }
}
