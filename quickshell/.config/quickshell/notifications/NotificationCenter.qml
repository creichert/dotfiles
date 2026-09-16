pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import Quickshell

PopupWindow {
    id: root

    required property var bar
    required property var config
    property var controller: null
    property bool open: false
    signal dismissed()
    implicitWidth: config.notificationWidth
    implicitHeight: config.notificationCenterHeight
    visible: open && controller !== null
    grabFocus: true
    color: "transparent"

    anchor.window: bar
    anchor.rect.x: bar.width - width - config.notificationMargin
    anchor.rect.y: bar.height + config.notificationMargin

    onVisibleChanged: {
        if (!visible)
            dismissed()
    }

    Connections {
        target: root.controller

        function onInteracted() {
            root.dismissed()
        }
    }

    FocusScope {
        anchors.fill: parent
        focus: true

        Keys.onEscapePressed: event => {
            root.dismissed()
            event.accepted = true
        }

        Rectangle {
            anchors.fill: parent
            radius: root.config.surfaceRadius
            color: root.config.notificationBackgroundColor
            border.width: 1
            border.color: root.config.accentColor

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
