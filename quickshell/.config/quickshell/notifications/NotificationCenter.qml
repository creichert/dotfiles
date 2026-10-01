pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var config
    required property var theme
    property var controller: null
    property bool open: false
    visible: open && controller !== null
    implicitWidth: Math.min(config.notificationCenterWidth, availableWidth)
    implicitHeight: Math.min(config.notificationCenterHeight, availableHeight)
    edgeInset: config.notificationMargin
    gap: config.notificationMargin

    onVisibleChanged: {
        if (visible) {
            Qt.callLater(() => {
                if (root.visible)
                    neutralFocus.forceActiveFocus()
            })
        }
    }

    Connections {
        target: root.controller

        function onInteracted() {
            root.dismissed()
        }
    }

    Rectangle {
        id: center

        anchors.fill: parent
        radius: root.theme.surfaceRadius
        color: root.theme.surface
        border.width: 1
        border.color: root.theme.separator

        // Consume clicks on inactive card space so only outside clicks dismiss.
        MouseArea {
            anchors.fill: parent
        }

        FocusScope {
            id: centerFocus

            anchors {
                fill: parent
                margins: root.theme.panelPadding
            }
            focus: true

            // A real child target avoids restoring the FocusScope's last control.
            Item {
                id: neutralFocus
                activeFocusOnTab: false
            }

            Keys.onEscapePressed: event => {
                root.dismissed()
                event.accepted = true
            }

            RowLayout {
                id: header

                anchors.top: parent.top
                width: parent.width
                spacing: root.theme.spacingMedium

                Text {
                    Layout.fillWidth: true
                    Layout.minimumWidth: 0
                    Layout.alignment: Qt.AlignVCenter
                    text: "Notifications"
                    color: root.theme.primaryText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.titleFontPixelSize + 2
                    font.bold: true
                    elide: Text.ElideRight
                }

                Components.Button {
                    id: clearButton

                    Layout.alignment: Qt.AlignVCenter
                    text: "Clear"
                    theme: root.theme
                    fontPixelSize: root.theme.bodyFontPixelSize
                    enabled: root.controller && root.controller.history.length > 0
                    onClicked: root.controller.clearHistory()
                }

                Components.Switch {
                    id: dndSwitch

                    Layout.alignment: Qt.AlignVCenter
                    theme: root.theme
                    text: "Do Not Disturb"
                    checked: root.controller ? root.controller.doNotDisturb : false
                    onToggled: {
                        if (root.controller)
                            root.controller.doNotDisturb = checked
                    }
                }
            }

            Flickable {
                id: historyView

                anchors {
                    top: header.bottom
                    topMargin: root.theme.spacingLarge * 2 + 1
                    left: parent.left
                    right: parent.right
                    bottom: parent.bottom
                }
                contentWidth: width
                contentHeight: records.implicitHeight
                clip: true
                interactive: contentHeight > height

                function revealControl(control) {
                    const position = control.mapToItem(records, 0, 0)
                    const bottom = position.y + control.height
                    if (position.y < contentY)
                        contentY = position.y
                    else if (bottom > contentY + height)
                        contentY = bottom - height
                }

                Column {
                    id: records

                    width: historyView.width
                    spacing: root.theme.spacingLarge

                    Repeater {
                        model: root.controller ? root.controller.history.slice().reverse() : []

                        delegate: NotificationRecord {
                            required property var modelData

                            width: records.width
                            theme: root.theme
                            controller: root.controller
                            record: modelData
                            onControlFocused: control => historyView.revealControl(control)
                        }
                    }
                }
            }

            Column {
                anchors.centerIn: historyView
                visible: root.controller && root.controller.history.length === 0
                spacing: root.theme.spacingLarge

                Components.Icon {
                    anchors.horizontalCenter: parent.horizontalCenter
                    theme: root.theme
                    name: "bell"
                    color: root.theme.mutedText
                    font.pixelSize: 40
                }

                Text {
                    text: "No notifications"
                    color: root.theme.mutedText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.bodyFontPixelSize
                }
            }
        }
    }
}
