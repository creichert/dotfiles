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
    readonly property rect triggerRect: {
        if (!triggerWindow || !triggerWindow.backingWindowVisible)
            return Qt.rect(0, 0, 0, 0)

        // itemRect() is not reactive; these dependencies refresh its position
        // when the bar or the trigger moves or changes size.
        triggerWindow.windowTransform
        trigger.x
        trigger.y
        trigger.width
        trigger.height
        return triggerWindow.itemRect(trigger)
    }
    readonly property int edgeInset: 8

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
