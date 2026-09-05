import QtQuick

Rectangle {
    id: root

    required property var workspace
    property bool showSpecial: false

    readonly property bool special: workspace.name.indexOf("special:") === 0
    readonly property var monitorState: workspace.monitor ? workspace.monitor.lastIpcObject : null
    readonly property bool specialActive: special && monitorState
        && monitorState.specialWorkspace
        && monitorState.specialWorkspace.name === workspace.name
    readonly property bool active: workspace.focused || specialActive
    readonly property string displayName: workspace.name.replace("special:", "")

    function icon() {
        if (workspace.urgent)
            return ""

        switch (displayName) {
        case "1": return ""
        case "2": return ""
        case "3": return ""
        case "4": return ""
        case "cfg":
        case "terms":
        case "db": return ""
        default: return ""
        }
    }

    visible: special === showSpecial && (!special || specialActive)
    implicitWidth: workspaceLabel.implicitWidth + 10
    implicitHeight: 30
    color: workspace.urgent ? "#eb4d4b" : active ? "#64727d" : "transparent"

    Text {
        id: workspaceLabel
        anchors.centerIn: parent
        text: `${root.displayName}: ${root.icon()}`
        color: "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.workspace.activate()
    }
}
