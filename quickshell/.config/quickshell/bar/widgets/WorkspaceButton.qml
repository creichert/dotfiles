import QtQuick

Rectangle {
    id: root

    required property var config
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
    implicitWidth: workspaceLabel.implicitWidth + config.workspaceHorizontalPadding
    implicitHeight: config.barHeight
    color: workspace.urgent ? config.urgentBackgroundColor
        : active ? config.activeBackgroundColor
        : "transparent"

    Text {
        id: workspaceLabel
        anchors.centerIn: parent
        text: `${root.displayName}: ${root.icon()}`
        color: root.config.textColor
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.workspace.activate()
    }
}
