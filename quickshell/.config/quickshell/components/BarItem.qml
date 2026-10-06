import QtQuick

Rectangle {
    id: root

    required property var config
    required property var theme
    required property Item contentItem
    property bool engaged: false

    implicitWidth: contentHost.width + 2 * config.barStatusHorizontalInset
    implicitHeight: config.barHeight
    color: engaged ? theme.selectedSurface : "transparent"
    radius: theme.controlRadius

    // Only feature content determines size. Badges and input handlers remain
    // ordinary children of the whole module, outside this natural-size host.
    Item {
        id: contentHost

        anchors.centerIn: parent
        width: root.contentItem ? root.contentItem.implicitWidth : 0
        height: root.contentItem ? root.contentItem.implicitHeight : 0
    }

    Binding {
        target: root.contentItem
        property: "parent"
        value: contentHost
    }
}
