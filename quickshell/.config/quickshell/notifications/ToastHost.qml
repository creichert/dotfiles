pragma ComponentBehavior: Bound

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

    margins {
        top: config.barHeight + config.notificationMargin
        right: config.notificationMargin
    }

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
