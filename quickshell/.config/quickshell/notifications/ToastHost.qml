pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import Quickshell
import Quickshell.Wayland

PanelWindow {
    id: root

    required property var config
    required property var controller
    implicitWidth: config.notificationWidth
    implicitHeight: toastColumn.implicitHeight
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    WlrLayershell.layer: WlrLayer.Overlay

    anchors {
        top: true
        right: true
    }

    // qmllint disable unqualified
    // qmllint disable unresolved-type
    margins.top: config.barHeight + config.notificationMargin
    margins.right: config.notificationMargin
    // qmllint enable unresolved-type
    // qmllint enable unqualified

    Column {
        id: toastColumn

        width: root.config.notificationWidth
        spacing: root.config.notificationSpacing

        Repeater {
            model: root.controller.visibleNotifications

            delegate: Toast {
                required property var modelData
                width: toastColumn.width
                config: root.config
                controller: root.controller
                notification: modelData
            }
        }
    }
}
