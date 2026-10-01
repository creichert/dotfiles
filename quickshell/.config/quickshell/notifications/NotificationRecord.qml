pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Widgets
import "../components" as Components

Rectangle {
    id: root

    required property var theme
    required property var controller
    required property var record
    signal controlFocused(var control)
    implicitHeight: content.implicitHeight + theme.spacingLarge * 2
    radius: theme.surfaceRadius
    color: record.urgency === 2 ? theme.urgent : theme.raisedSurface
    border.width: 1
    border.color: activeFocus ? theme.activeAccent : theme.separator
    activeFocusOnTab: defaultActionAvailable

    onActiveFocusChanged: {
        if (activeFocus)
            root.controlFocused(root)
    }

    Keys.onPressed: event => {
        // Child controls handle their own activation; never invoke both actions.
        if (!root.activeFocus || !root.defaultActionAvailable)
            return

        if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter || event.key === Qt.Key_Space) {
            event.accepted = true
            root.controller.invokeDefaultAction(root.record.id)
        }
    }

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
    readonly property bool defaultActionAvailable: hasDefaultAction()
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
        enabled: root.defaultActionAvailable
        onClicked: root.controller.invokeDefaultAction(root.record.id)
    }

    Column {
        id: content

        anchors {
            fill: parent
            margins: root.theme.spacingLarge
        }
        spacing: root.theme.spacingMedium

        Row {
            width: parent.width
            spacing: root.theme.spacingMedium

            IconImage {
                id: appIcon

                source: root.appIconFailed ? root.fallbackIconSource() : root.appIconSource
                implicitSize: 32
                onStatusChanged: {
                    if (status === Image.Error && !root.appIconFailed)
                        root.appIconFailed = true
                }
            }

            Column {
                width: parent.width - appIcon.width - controls.width - parent.spacing * 2
                spacing: root.theme.spacingSmall

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.record.summary
                    color: root.theme.primaryText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.bodyFontPixelSize + 2
                    font.bold: root.record.unread
                }

                Text {
                    width: parent.width
                    elide: Text.ElideRight
                    text: root.record.appName
                    color: root.theme.mutedText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.secondaryFontPixelSize
                }
            }

            Row {
                id: controls
                spacing: root.theme.spacingMedium

                Text {
                    text: root.timestamp()
                    color: root.theme.mutedText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.secondaryFontPixelSize
                }

                Components.IconButton {
                    id: readButton

                    theme: root.theme
                    iconName: root.record.unread ? "markRead" : "markUnread"
                    text: root.record.unread ? "Mark read" : "Mark unread"
                    iconPixelSize: root.theme.bodyFontPixelSize + 4
                    implicitWidth: 36
                    implicitHeight: 36
                    onActiveFocusChanged: {
                        if (activeFocus)
                            root.controlFocused(readButton)
                    }
                    onClicked: root.controller.setRead(root.record.id, !root.record.unread)
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
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.bodyFontPixelSize

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
            visible: actionRepeater.count > 0
            spacing: root.theme.spacingMedium

            Repeater {
                id: actionRepeater

                model: root.actionButtonsAvailable
                    ? root.controller.nonDefaultActionsFor(root.record.id)
                    : []

                delegate: Components.Button {
                    id: actionButton

                    required property var modelData
                    theme: root.theme
                    text: actionButton.modelData.text
                    fontPixelSize: root.theme.bodyFontPixelSize
                    onActiveFocusChanged: {
                        if (activeFocus)
                            root.controlFocused(actionButton)
                    }
                    onClicked: root.controller.invokeAction(root.record.id, actionButton.modelData.identifier)
                }
            }
        }
    }
}
