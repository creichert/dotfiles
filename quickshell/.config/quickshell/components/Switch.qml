import QtQuick
import QtQuick.Controls as Controls

Controls.Switch {
    id: root

    required property var theme

    implicitWidth: 40
    implicitHeight: 28
    padding: 0
    spacing: 0
    hoverEnabled: true
    focusPolicy: Qt.StrongFocus

    Keys.onReturnPressed: event => {
        root.click()
        event.accepted = true
    }
    Keys.onEnterPressed: event => {
        root.click()
        event.accepted = true
    }

    contentItem: Item {}

    indicator: Rectangle {
        x: (root.width - width) / 2
        y: (root.height - height) / 2
        width: 36
        height: 20
        radius: height / 2
        color: root.checked ? root.theme.activeAccent : root.theme.selectedSurface
        border.width: 1
        border.color: root.hovered ? root.theme.primaryText : root.theme.separator
        opacity: root.pressed ? 0.85 : 1

        Behavior on color {
            ColorAnimation { duration: 130 }
        }

        Rectangle {
            anchors.fill: parent
            anchors.margins: -3
            radius: height / 2
            color: "transparent"
            border.width: 1
            border.color: root.theme.primaryText
            visible: root.visualFocus
        }

        Rectangle {
            width: 14
            height: 14
            x: root.checked ? parent.width - width - 3 : 3
            y: 3
            radius: height / 2
            color: root.checked ? root.theme.surface : root.theme.primaryText

            Behavior on color {
                ColorAnimation { duration: 130 }
            }

            Behavior on x {
                NumberAnimation { duration: 130; easing.type: Easing.OutCubic }
            }
        }
    }
}
