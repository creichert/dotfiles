import QtQuick

FocusScope {
    id: root

    required property AnchoredPanel panel
    required property var theme
    default property alias contentData: contentHost.data

    anchors.fill: parent
    focus: true

    Keys.priority: Keys.AfterItem
    Keys.onEscapePressed: event => {
        // Dismissal may immediately unload this body.
        event.accepted = true
        root.panel.dismissed()
    }

    Component.onCompleted: {
        // LazyLoader can create the panel with visible already true.
        if (panel.visible)
            focusTimer.start()
    }

    // Internal objects must not enter the consumer's padded content host.
    data: [
        Rectangle {
            anchors.fill: parent
            radius: root.theme.surfaceRadius
            color: root.theme.surface
            border.width: 1
            border.color: root.theme.separator

            // Consume blank-space clicks underneath feature controls.
            MouseArea {
                anchors.fill: parent
            }
        },
        Item {
            id: contentHost

            anchors.fill: parent
            anchors.margins: root.theme.panelPadding
        },
        Timer {
            id: focusTimer

            interval: 0
            repeat: false
            onTriggered: {
                // Do not steal focus from an already-focused descendant.
                if (root.panel.visible && root.visible && !root.activeFocus)
                    root.forceActiveFocus()
            }
        },
        Connections {
            target: root.panel

            function onVisibleChanged() {
                if (root.panel.visible)
                    focusTimer.restart()
                else
                    focusTimer.stop()
            }
        }
    ]
}
