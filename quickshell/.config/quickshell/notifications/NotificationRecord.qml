pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Widgets

Rectangle {
    id: root

    required property var config
    required property var controller
    required property var record
    implicitHeight: content.implicitHeight + 20
    radius: 6
    color: record.urgency === 2 ? config.urgentBackgroundColor : config.notificationBackgroundColor
    border.width: 1
    border.color: config.accentColor

    function bodyText(body) {
        return body.replace(/<img\b[^>]*>/gi, "")
    }

    function openLink(link) {
        const match = link.match(/^([a-z][a-z0-9+.-]*):/i)

        if (match && (match[1].toLowerCase() === "http" || match[1].toLowerCase() === "https")) {
            return Qt.openUrlExternally(link)
        }

        return false
    }

    function timestamp() {
        const date = new Date(record.timestamp)
        const now = new Date()

        if (date.toDateString() === now.toDateString())
            return Qt.formatDateTime(date, "HH:mm")
        if (date.getFullYear() === now.getFullYear())
            return Qt.formatDateTime(date, "MMM d HH:mm")
        return Qt.formatDateTime(date, "MMM d yyyy HH:mm")
    }

    function hasDefaultAction() {
        for (const action of controller.actionsFor(record.id)) {
            if (action.identifier === "default")
                return true
        }

        return false
    }

    function iconSource() {
        const appIcon = record.appIcon || ""

        if (appIcon.startsWith("file:"))
            return appIcon
        if (appIcon.startsWith("/"))
            return "file://" + appIcon
        if (appIcon.length > 0)
            return Quickshell.iconPath(appIcon, "application-x-executable")
        return fallbackIconSource()
    }

    function fallbackIconSource() {
        const desktopEntry = record.desktopEntry || ""

        if (desktopEntry.length > 0)
            return Quickshell.iconPath(desktopEntry, "application-x-executable")
        return Quickshell.iconPath("application-x-executable", true)
    }

    property bool actionButtonsExpired: false
    property bool appIconFailed: false
    readonly property string appIconSource: iconSource()
    readonly property real actionButtonsExpireAt: record.actionButtonsExpireAt || 0
    readonly property bool actionButtonsAvailable: actionButtonsExpireAt === 0
        || (!actionButtonsExpired && Date.now() < actionButtonsExpireAt)

    onAppIconSourceChanged: appIconFailed = false

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
        enabled: root.hasDefaultAction()
        onClicked: root.controller.invokeDefaultAction(root.record.id)
    }

    Column {
        id: content

        anchors {
            fill: parent
            margins: 10
        }
        spacing: 6

        Row {
            width: parent.width
            spacing: 8

            IconImage {
                id: appIcon

                source: root.appIconFailed ? root.fallbackIconSource() : root.appIconSource
                implicitSize: 20
                onStatusChanged: {
                    if (status === Image.Error && !root.appIconFailed)
                        root.appIconFailed = true
                }
            }

            Column {
                width: parent.width - appIcon.width - controls.width - parent.spacing * 2
                spacing: 1

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.record.summary
                    color: root.config.textColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize
                    font.bold: root.record.unread
                }

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.record.appName + " - " + root.timestamp()
                    color: root.config.textColor
                    opacity: 0.7
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize - 3
                }
            }

            Row {
                id: controls

                spacing: 6

                Text {
                    width: 12
                    text: root.record.unread ? "●" : "○"
                    color: root.config.textColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize - 3

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.controller.setRead(root.record.id, !root.record.unread)
                    }
                }

                Text {
                    width: 12
                    text: "x"
                    color: root.config.textColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.fontPixelSize - 2

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.controller.dismissHistoryRecord(root.record.id)
                    }
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
            text: root.bodyText(root.record.body)
            textFormat: Text.StyledText
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize - 1

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true
                cursorShape: parent.linkAt(mouseX, mouseY).length > 0
                    ? Qt.PointingHandCursor : Qt.ArrowCursor
                onClicked: mouse => {
                    const link = parent.linkAt(mouse.x, mouse.y)

                    if (link.length > 0) {
                        if (root.openLink(link))
                            Qt.callLater(() => root.controller.notificationInteracted())
                        return
                    }

                    root.controller.invokeDefaultAction(root.record.id)
                }
            }
        }

        Flow {
            width: parent.width
            spacing: 6

            Repeater {
                model: root.actionButtonsAvailable
                    ? root.controller.nonDefaultActionsFor(root.record.id)
                    : []

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
                        font.pixelSize: root.config.fontPixelSize - 2
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: root.controller.invokeAction(root.record.id, actionButton.modelData.identifier)
                    }
                }
            }
        }
    }
}
