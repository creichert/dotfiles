pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Wayland
import "../components" as Components

PanelWindow {
    id: root

    required property var bar
    required property var config
    required property var theme
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
        if (visible) {
            dndSwitch.focus = false
            Qt.callLater(() => {
                if (root.visible)
                    centerFocus.forceActiveFocus()
            })
        } else {
            dismissed()
        }
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

                    RowLayout {
                        id: controls

                        spacing: 10

                        Item {
                            Layout.alignment: Qt.AlignVCenter
                            implicitWidth: dndContent.implicitWidth + root.theme.spacingMedium * 2
                            implicitHeight: dndContent.implicitHeight + root.theme.spacingSmall * 2

                            MouseArea {
                                anchors.fill: parent
                                onClicked: dndSwitch.click()
                            }

                            RowLayout {
                                id: dndContent

                                anchors.centerIn: parent
                                spacing: root.theme.spacingMedium

                                Text {
                                    Layout.alignment: Qt.AlignVCenter
                                    text: "Do Not Disturb"
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.fontPixelSize - 2
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
                        }

                        Text {
                            Layout.alignment: Qt.AlignVCenter
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
