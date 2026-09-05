import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets

Item {
    id: root

    implicitHeight: 30
    implicitWidth: titleRow.implicitWidth
    clip: true

    readonly property var toplevel: Hyprland.activeToplevel
    readonly property var hyprlandToplevel: Hyprland.toplevels.values.find(window => window.activated)
    readonly property string appId: toplevel && toplevel.appId
        ? toplevel.appId
        : hyprlandToplevel && hyprlandToplevel.lastIpcObject
            ? hyprlandToplevel.lastIpcObject.class || ""
            : ""
    readonly property string iconSource: appId.length > 0
        ? Quickshell.iconPath(appId, true)
        : ""

    Row {
        id: titleRow
        anchors.centerIn: parent
        spacing: 6
        property var currentToplevel: root.toplevel
        property string iconSource: root.iconSource.length > 0
            ? root.iconSource
            : root.toplevel ? Quickshell.iconPath("application-x-executable", true) : ""

        IconImage {
            visible: titleRow.iconSource.length > 0
            source: titleRow.iconSource
            implicitSize: 16
        }

        Text {
            width: Math.min(800, implicitWidth)
            elide: Text.ElideRight
            text: titleRow.currentToplevel ? titleRow.currentToplevel.title : ""
            color: "white"
            font.pixelSize: 14
        }
    }
}
