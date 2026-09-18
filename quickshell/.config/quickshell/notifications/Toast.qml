pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Services.Notifications
import Quickshell.Widgets

Rectangle {
    id: root

    required property var config
    required property var controller
    required property var notification
    implicitHeight: content.implicitHeight + 24
    radius: config.surfaceRadius
    color: notification.urgency === NotificationUrgency.Critical
        ? config.urgentBackgroundColor
        : config.notificationBackgroundColor
    border.width: 1
    border.color: config.accentColor

    function bodyText(body) {
        // StyledText can load body images, which are intentionally unsupported.
        return body.replace(/<img\b[^>]*>/gi, "")
    }

    function openLink(link) {
        const match = link.match(/^([a-z][a-z0-9+.-]*):/i)

        if (!match)
            return

        const scheme = match[1].toLowerCase()

        if (scheme === "http" || scheme === "https") {
            Qt.openUrlExternally(link)
            root.controller.notificationInteracted()
        }
    }

    function iconSource() {
        if (notification.appIcon.startsWith("file:"))
            return notification.appIcon
        if (notification.appIcon.startsWith("/"))
            return "file://" + notification.appIcon
        if (notification.appIcon.length > 0)
            return Quickshell.iconPath(notification.appIcon, "application-x-executable")
        return fallbackIconSource()
    }

    function fallbackIconSource() {
        if (notification.desktopEntry.length > 0)
            return Quickshell.iconPath(notification.desktopEntry, "application-x-executable")
        return Quickshell.iconPath("application-x-executable", true)
    }

    property real actionButtonsExpireAt: {
        const record = controller.historyRecord(notification.id)
        return record && record.actionButtonsExpireAt ? record.actionButtonsExpireAt : 0
    }
    property bool actionButtonsExpired: false
    property bool appIconFailed: false
    readonly property string appIconSource: iconSource()
    readonly property bool actionButtonsAvailable: actionButtonsExpireAt === 0
        || (!actionButtonsExpired && Date.now() < actionButtonsExpireAt)

    onActionButtonsExpireAtChanged: actionButtonsExpired = false
    onAppIconSourceChanged: appIconFailed = false

    Timer {
        id: toastTimer

        interval: root.config.notificationToastTimeout
        repeat: false
        running: root.notification.urgency !== NotificationUrgency.Critical
        // Hiding a toast does not remove its notification-center record.
        onTriggered: root.controller.hideNotification(root.notification)
    }

    Connections {
        target: root.controller

        function onNotificationUpdated(notification) {
            if (notification.id === root.notification.id
                    && notification.urgency !== NotificationUrgency.Critical)
                toastTimer.restart()
        }
    }

    Timer {
        interval: Math.max(1, root.actionButtonsExpireAt - Date.now())
        repeat: false
        running: root.actionButtonsExpireAt > Date.now()
        onTriggered: root.actionButtonsExpired = true
    }

    // This sits behind explicit controls, so a button or link cannot also
    // invoke the notification's default action.
    MouseArea {
        anchors.fill: parent
        enabled: root.controller.actionsFor(root.notification.id).some(action => action.identifier === "default")
        onClicked: root.controller.invokeDefaultAction(root.notification.id)
    }

    Column {
        id: content

        anchors {
            fill: parent
            margins: 12
        }
        spacing: 8

        Row {
            width: parent.width
            spacing: 8

            IconImage {
                id: appIcon

                visible: source.toString().length > 0
                source: root.appIconFailed ? root.fallbackIconSource() : root.appIconSource
                implicitSize: 24
                onStatusChanged: {
                    if (status === Image.Error && !root.appIconFailed)
                        root.appIconFailed = true
                }
            }

            Column {
                width: parent.width - appIcon.width - closeButton.width - parent.spacing * 2
                spacing: 2

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.notification.summary
                    color: root.config.textColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize
                    font.bold: true
                }

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.notification.appName
                    color: root.config.textColor
                    opacity: 0.8
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize - 2
                }
            }

            Text {
                id: closeButton

                width: 16
                text: "x"
                color: root.config.textColor
                opacity: 0.8
                horizontalAlignment: Text.AlignRight
                font.family: root.config.fontFamily
                font.pixelSize: root.config.fontPixelSize

                MouseArea {
                    anchors.fill: parent
                    onClicked: root.controller.dismissNotification(root.notification)
                }
            }
        }

        Text {
            id: body

            width: parent.width
            visible: text.length > 0
            wrapMode: Text.Wrap
            maximumLineCount: 4
            elide: Text.ElideRight
            text: root.bodyText(root.notification.body)
            textFormat: Text.StyledText
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: parent.linkAt(mouseX, mouseY).length > 0
                    ? Qt.PointingHandCursor : Qt.ArrowCursor
                onClicked: mouse => {
                    const link = parent.linkAt(mouse.x, mouse.y)

                    if (link.length > 0) {
                        root.openLink(link)
                        return
                    }

                    root.controller.invokeDefaultAction(root.notification.id)
                }
            }
        }

        Flow {
            width: parent.width
            spacing: 6

            Repeater {
                model: root.actionButtonsAvailable
                    ? root.controller.nonDefaultActionsFor(root.notification.id)
                    : []

                delegate: Rectangle {
                    id: actionButton

                    required property var modelData
                    implicitWidth: actionLabel.implicitWidth + 16
                    implicitHeight: actionLabel.implicitHeight + 8
                    radius: root.config.controlRadius
                    color: Qt.rgba(1, 1, 1, 0.12)

                    Text {
                        id: actionLabel

                        anchors.centerIn: parent
                        text: actionButton.modelData.text
                        color: root.config.textColor
                        font.family: root.config.fontFamily
                        font.pixelSize: root.config.fontPixelSize - 1
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.controller.invokeAction(root.notification.id, actionButton.modelData.identifier)
                    }
                }
            }
        }
    }
}
