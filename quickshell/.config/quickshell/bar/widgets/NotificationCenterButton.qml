import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    property var controller: null
    readonly property bool centerVisible: controller && controller.notificationCenterVisible
    readonly property int unreadCount: {
        if (!controller)
            return 0

        let count = 0

        for (const record of controller.history) {
            if (record.unread)
                count++
        }

        return count
    }

    visible: controller !== null
    engaged: centerVisible || Boolean(controller && controller.doNotDisturb)

    contentItem: Item {
        implicitWidth: Math.max(bellIcon.implicitWidth, mutedBellIcon.implicitWidth)
        implicitHeight: Math.max(bellIcon.implicitHeight, mutedBellIcon.implicitHeight)

        Components.Icon {
            id: bellIcon

            anchors.centerIn: parent
            visible: !root.controller || !root.controller.doNotDisturb
            name: "bell"
            theme: root.theme
        }

        Components.Icon {
            id: mutedBellIcon

            anchors.centerIn: parent
            visible: Boolean(root.controller && root.controller.doNotDisturb)
            name: "bellMuted"
            theme: root.theme
        }
    }

    Rectangle {
        visible: root.unreadCount > 0
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 3
        width: unreadLabel.implicitWidth + 6
        height: unreadLabel.implicitHeight + 2
        radius: height / 2
        color: root.theme.urgent

        Text {
            id: unreadLabel

            anchors.centerIn: parent
            text: root.unreadCount > 99 ? "99+" : root.unreadCount
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize - 4
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            if (root.controller)
                root.controller.notificationCenterVisible = !root.controller.notificationCenterVisible
        }
    }
}
