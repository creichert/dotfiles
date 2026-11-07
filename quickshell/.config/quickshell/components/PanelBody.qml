import QtQuick

// Anchored-panel interior; feature content is parented into a padded host.
// Handle escapePressed synchronously: accept before navigation or dismissal,
// otherwise the body emits panel.dismissed(). Do not retain the event.
FocusScope {
    id: root

    required property AnchoredPanel panel
    required property var theme
    property Item initialFocusItem: root
    default property alias contentData: contentHost.data
    signal escapePressed(var event)

    anchors.fill: parent
    focus: true

    Keys.priority: Keys.AfterItem
    Keys.onEscapePressed: event => {
        // Specific Keys handlers arrive accepted; reset for opt-in interception.
        event.accepted = false
        root.escapePressed(event)
        if (!event.accepted) {
            // Dismissal may immediately unload this body.
            event.accepted = true
            root.panel.dismissed()
        }
    }

    // Schedule the opening-focus policy, normally retaining focused descendants.
    function focusContent() {
        if (panel.visible)
            focusTimer.restart()
    }

    Component.onCompleted: {
        // LazyLoader can create the panel with visible already true.
        focusContent()
    }

    // Bypass the default content alias: chrome and infrastructure belong to the
    // body itself, not inside the consumer's padded content host.
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

            // Defer past construction; close cancels the request, and destruction
            // of the body also destroys this timer.
            interval: 0
            repeat: false
            onTriggered: {
                // Preserve descendant focus by default; an explicit opening
                // target instead resets the scope's previously focused control.
                if (root.panel.visible && root.visible && root.initialFocusItem
                        && (root.initialFocusItem !== root || !root.activeFocus))
                    root.initialFocusItem.forceActiveFocus()
            }
        },
        Connections {
            target: root.panel

            function onVisibleChanged() {
                if (root.panel.visible)
                    root.focusContent()
                else
                    focusTimer.stop()
            }
        }
    ]
}
