import QtQuick
import QtQuick.Controls as Controls

Button {
    id: root

    required property string iconName

    implicitWidth: 28
    implicitHeight: 28
    leftPadding: 0
    rightPadding: 0
    topPadding: 0
    bottomPadding: 0

    contentItem: Icon {
        theme: root.theme
        name: root.iconName
        color: root.enabled ? root.theme.primaryText : root.theme.mutedText
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
    }

    Controls.ToolTip.visible: root.enabled && root.hovered && root.text.length > 0
    Controls.ToolTip.delay: 500
    Controls.ToolTip.text: root.text
}
