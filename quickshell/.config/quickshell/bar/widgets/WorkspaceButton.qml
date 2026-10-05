import QtQuick
import QtQuick.Layouts
import "../../components" as Components

Rectangle {
    id: root

    required property var config
    required property var theme
    required property var workspace

    readonly property int workspaceButtonWidth: 32
    property bool showSpecial: false

    readonly property bool special: workspace.name.indexOf("special:") === 0
    readonly property var monitorState: workspace.monitor ? workspace.monitor.lastIpcObject : null
    readonly property bool specialActive: Boolean(special && monitorState
        && monitorState.specialWorkspace
        && monitorState.specialWorkspace.name === workspace.name)
    readonly property bool active: workspace.focused || specialActive
    readonly property string displayName: workspace.name.replace("special:", "")

    visible: special === showSpecial && (!special || specialActive)
    implicitWidth: workspaceButtonWidth
    implicitHeight: config.barHeight
    // Fix the actual navigation slot for both normal and special workspaces.
    Layout.minimumWidth: root.workspaceButtonWidth
    Layout.preferredWidth: root.workspaceButtonWidth
    Layout.maximumWidth: root.workspaceButtonWidth
    radius: theme.controlRadius
    color: workspace.urgent ? theme.urgent
        : active ? theme.selectedSurface
        : "transparent"

    Row {
        id: workspaceRow
        anchors.centerIn: parent
        spacing: 0

        Components.Icon {
            name: root.config.workspaceIcons[root.displayName] || root.config.workspaceIcons.default
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.workspace.activate()
    }
}
