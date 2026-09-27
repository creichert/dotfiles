import QtQuick
import QtQuick.Controls as Controls

Controls.Slider {
    id: root

    required property var theme

    implicitHeight: 32
    from: 0
    to: 1
    stepSize: 0.01
    hoverEnabled: true
    focusPolicy: Qt.StrongFocus
    wheelEnabled: true

    background: Rectangle {
        x: root.leftPadding
        y: (root.height - height) / 2
        width: root.availableWidth
        height: 4
        radius: height / 2
        color: root.theme.selectedSurface

        Rectangle {
            width: root.visualPosition * parent.width
            height: parent.height
            radius: parent.radius
            color: root.enabled ? root.theme.activeAccent : root.theme.mutedText
        }
    }

    handle: Rectangle {
        x: root.leftPadding + root.visualPosition * (root.availableWidth - width)
        y: (root.height - height) / 2
        implicitWidth: 16
        implicitHeight: 16
        radius: width / 2
        color: root.enabled ? root.theme.primaryText : root.theme.mutedText
        border.width: root.visualFocus ? 2 : 1
        border.color: root.visualFocus ? root.theme.activeAccent
            : root.pressed ? root.theme.primaryText : root.theme.separator
        scale: root.pressed ? 1.15 : root.hovered ? 1.08 : 1
    }
}
