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
    radius: 6
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

    TapHandler {
        onTapped: root.controller.invokeDefaultAction(root.notification.id)
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
                source: root.notification.appIcon.length > 0
                    ? Quickshell.iconPath(root.notification.appIcon, "application-x-executable")
                    : Quickshell.iconPath("application-x-executable", true)
                implicitSize: 24
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

                TapHandler {
                    onTapped: root.controller.dismissNotification(root.notification)
                }
            }
        }

        Text {
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
            onLinkActivated: link => root.openLink(link)
        }

        Flow {
            width: parent.width
            spacing: 6

            Repeater {
                model: root.controller.nonDefaultActionsFor(root.notification.id)

                delegate: Rectangle {
                    id: actionButton

                    required property var modelData
                    implicitWidth: actionLabel.implicitWidth + 16
                    implicitHeight: actionLabel.implicitHeight + 8
                    radius: 4
                    color: Qt.rgba(1, 1, 1, 0.12)

                    Text {
                        id: actionLabel

                        anchors.centerIn: parent
                        text: actionButton.modelData.text
                        color: root.config.textColor
                        font.family: root.config.fontFamily
                        font.pixelSize: root.config.fontPixelSize - 1
                    }

                    TapHandler {
                        onTapped: root.controller.invokeAction(root.notification.id, actionButton.modelData.identifier)
                    }
                }
            }
        }
    }
}
