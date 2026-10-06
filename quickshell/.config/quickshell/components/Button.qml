import QtQuick
import QtQuick.Controls as Controls

Controls.Button {
    id: root

    required property var theme
    property int fontPixelSize: theme.fontPixelSize - 2

    leftPadding: theme.spacingMedium
    rightPadding: theme.spacingMedium
    topPadding: theme.spacingSmall
    bottomPadding: theme.spacingSmall
    hoverEnabled: true
    focusPolicy: Qt.StrongFocus

    contentItem: Text {
        text: root.text
        color: root.enabled ? root.theme.primaryText : root.theme.mutedText
        font.family: root.theme.fontFamily
        font.pixelSize: root.fontPixelSize
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
    }

    background: Rectangle {
        radius: root.theme.controlRadius
        color: root.pressed ? root.theme.raisedSurface
            : root.hovered && root.enabled ? root.theme.selectedSurface : "transparent"
        border.width: 1
        border.color: root.visualFocus ? root.theme.activeAccent
            : root.hovered && root.enabled ? root.theme.mutedText : root.theme.separator
        opacity: root.enabled ? 1 : 0.5

        Behavior on color {
            ColorAnimation { duration: 100 }
        }
    }
}
