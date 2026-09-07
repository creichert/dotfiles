import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Widgets

Item {
    id: root

    required property var config
    implicitHeight: config.barHeight
    implicitWidth: titleRow.implicitWidth
    clip: true

    readonly property var toplevel: Hyprland.activeToplevel
    readonly property var waylandToplevel: toplevel ? toplevel.wayland : null
    readonly property string appId: waylandToplevel ? waylandToplevel.appId || "" : ""
    readonly property string iconSource: appId.length > 0
        ? Quickshell.iconPath(appId, "application-x-executable")
        : Quickshell.iconPath("application-x-executable", true)

    Row {
        id: titleRow
        anchors.centerIn: parent
        spacing: root.config.titleSpacing

        IconImage {
            visible: root.iconSource.length > 0
            source: root.iconSource
            implicitSize: 16
        }

        Text {
            width: Math.min(800, implicitWidth)
            elide: Text.ElideRight
            text: root.toplevel ? root.toplevel.title : ""
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }
    }
}
