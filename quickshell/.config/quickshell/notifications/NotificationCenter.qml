pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import Quickshell
import Quickshell.Wayland

PanelWindow {
    id: root

    required property var bar
    required property var config
    property var controller: null
    property bool open: false
    signal dismissed()
    screen: bar.screen
    visible: open && controller !== null
    color: "transparent"
    exclusionMode: ExclusionMode.Ignore
    focusable: true

    anchors {
        top: true
        bottom: true
        left: true
        right: true
    }

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    onVisibleChanged: {
        if (visible)
            Qt.callLater(() => centerFocus.forceActiveFocus())
        else
            dismissed()
    }

    Connections {
        target: root.controller

        function onInteracted() {
            root.dismissed()
        }
    }

    // Match native popup dismissal without relying on an input-grabbing
    // xdg_popup, which cannot be opened from IPC.
    MouseArea {
        anchors.fill: parent
        onClicked: root.dismissed()
    }

    Rectangle {
        id: center

        width: root.config.notificationWidth
        height: root.config.notificationCenterHeight
        anchors {
            top: parent.top
            right: parent.right
            topMargin: root.bar.height + root.config.notificationMargin
            rightMargin: root.config.notificationMargin
        }
        radius: root.config.surfaceRadius
        color: root.config.notificationBackgroundColor
        border.width: 1
        border.color: root.config.accentColor

        // Consume clicks on inactive card space so only outside clicks dismiss.
        MouseArea {
            anchors.fill: parent
        }

        FocusScope {
            id: centerFocus

            anchors.fill: parent
            focus: true

            Keys.onEscapePressed: event => {
                root.dismissed()
                event.accepted = true
            }

            Column {
                anchors {
                    fill: parent
                    margins: 12
                }
                spacing: 8

                Row {
                    width: parent.width

                    Text {
                        width: parent.width - controls.width
                        text: "Notifications"
                        color: root.config.textColor
                        font.family: root.config.fontFamily
                        font.pixelSize: root.config.fontPixelSize
                        font.bold: true
                    }

                    Row {
                        id: controls

                        spacing: 10

                        Text {
                            text: root.controller && root.controller.doNotDisturb ? "DND on" : "DND off"
                            color: root.config.textColor
                            font.family: root.config.fontFamily
                            font.pixelSize: root.config.fontPixelSize - 2

                            MouseArea {
                                anchors.fill: parent
                                onClicked: root.controller.doNotDisturb = !root.controller.doNotDisturb
                            }
                        }

                        Text {
                            text: "Clear"
                            color: root.config.textColor
                            font.family: root.config.fontFamily
                            font.pixelSize: root.config.fontPixelSize - 2

                            MouseArea {
                                anchors.fill: parent
                                onClicked: root.controller.clearHistory()
                            }
                        }
                    }
                }

                Rectangle {
                    width: parent.width
                    height: 1
                    color: Qt.rgba(1, 1, 1, 0.16)
                }

                Flickable {
                    width: parent.width
                    height: parent.height - y
                    contentWidth: width
                    contentHeight: records.implicitHeight
                    clip: true

                    Column {
                        id: records

                        width: parent.width
                        spacing: root.config.notificationSpacing

                        Repeater {
                            model: root.controller ? root.controller.history.slice().reverse() : []

                            delegate: NotificationRecord {
                                required property var modelData

                                width: records.width
                                config: root.config
                                controller: root.controller
                                record: modelData
                            }
                        }

                        Text {
                            visible: root.controller && root.controller.history.length === 0
                            width: parent.width
                            text: "No notifications"
                            color: root.config.textColor
                            opacity: 0.8
                            horizontalAlignment: Text.AlignHCenter
                            font.family: root.config.fontFamily
                            font.pixelSize: root.config.fontPixelSize
                        }
                    }
                }
            }
        }
    }
}
