import QtQuick

Rectangle {
    id: root

    required property var config
    property var controller: null
    property bool centerVisible: false
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
    implicitWidth: 32
    implicitHeight: config.barHeight
    color: controller && controller.doNotDisturb
        ? config.inhibitedBackgroundColor
        : "transparent"

    Text {
        anchors.centerIn: parent
        text: root.controller && root.controller.doNotDisturb ? "" : ""
        color: root.controller && root.controller.doNotDisturb
            ? root.config.inhibitedTextColor
            : root.config.textColor
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }

    Rectangle {
        visible: root.unreadCount > 0
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: 3
        width: unreadLabel.implicitWidth + 6
        height: unreadLabel.implicitHeight + 2
        radius: height / 2
        color: root.config.urgentBackgroundColor

        Text {
            id: unreadLabel

            anchors.centerIn: parent
            text: root.unreadCount > 99 ? "99+" : root.unreadCount
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize - 4
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.centerVisible = !root.centerVisible
    }
}
