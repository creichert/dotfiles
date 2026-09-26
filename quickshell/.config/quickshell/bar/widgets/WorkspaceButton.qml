import QtQuick
import "../../components" as Components

Rectangle {
    id: root

    required property var config
    required property var theme
    required property var workspace
    property bool showSpecial: false

    readonly property bool special: workspace.name.indexOf("special:") === 0
    readonly property var monitorState: workspace.monitor ? workspace.monitor.lastIpcObject : null
    readonly property bool specialActive: Boolean(special && monitorState
        && monitorState.specialWorkspace
        && monitorState.specialWorkspace.name === workspace.name)
    readonly property bool active: workspace.focused || specialActive
    readonly property string displayName: workspace.name.replace("special:", "")

    function icon() {
        if (workspace.urgent)
            return config.workspaceIcons.urgent

        return config.workspaceIcons[displayName] || config.workspaceIcons.default
    }

    visible: special === showSpecial && (!special || specialActive)
    implicitWidth: workspaceRow.implicitWidth + config.workspaceHorizontalPadding
    implicitHeight: config.barHeight
    color: workspace.urgent ? theme.urgent
        : active ? theme.selectedSurface
        : "transparent"

    Row {
        id: workspaceRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: `${root.displayName}: `
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: root.icon()
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.workspace.activate()
    }
}
