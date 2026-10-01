// qmllint disable uncreatable-type

import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland

PanelWindow {
    id: root

    required property Item trigger
    property int gap: 0
    signal dismissed()

    readonly property var triggerWindow: trigger ? trigger.QsWindow.window : null
    readonly property rect triggerRect: placement.rect
    readonly property int edgeInset: 8

    QtObject {
        id: placement

        property bool ready: false
        property bool captured: false
        property rect rect: Qt.rect(0, 0, 0, 0)

        function capture() {
            if (!ready || captured || !root.visible || !root.triggerWindow
                    || !root.triggerWindow.backingWindowVisible)
                return

            // itemRect() is nonreactive; sample once, not through a binding.
            rect = root.triggerWindow.itemRect(root.trigger)
            captured = true
        }
    }

    Component.onCompleted: {
        // LazyLoader may create this panel with visible already true.
        placement.ready = true
        placement.capture()
    }

    Connections {
        target: root

        function onVisibleChanged() {
            if (root.visible)
                placement.capture()
            else
                placement.captured = false
        }

        function onTriggerWindowChanged() {
            placement.capture()
        }
    }

    Connections {
        target: root.triggerWindow
        // Startup/reload may attach the trigger before its bar is mapped.
        enabled: placement.ready && root.visible && !placement.captured

        function onBackingWindowVisibleChanged() {
            placement.capture()
        }
    }

    screen: triggerWindow ? triggerWindow.screen : null
    anchors.top: true
    anchors.left: true
    // Quickshell's generated qmltypes omit the panel margin group type.
    // qmllint disable unqualified
    // qmllint disable unresolved-type
    margins.left: screen ? Math.round(Math.max(edgeInset, Math.min(
        triggerRect.x + triggerRect.width / 2 - implicitWidth / 2,
        screen.width - implicitWidth - edgeInset))) : 0
    margins.top: screen ? Math.round(Math.max(edgeInset, Math.min(
        triggerRect.y + triggerRect.height + gap,
        screen.height - implicitHeight - edgeInset))) : 0
    // qmllint enable unresolved-type
    // qmllint enable unqualified
    exclusionMode: ExclusionMode.Ignore

    color: "transparent"
    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.OnDemand

    HyprlandFocusGrab {
        windows: [root]
        active: root.visible && root.backingWindowVisible
        onCleared: {
            if (root.visible)
                root.dismissed()
        }
    }
}
