pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import Quickshell
import Quickshell.Wayland
import Quickshell.Widgets

PanelWindow {
    id: root

    required property var config
    required property var controller
    screen: controller.targetScreen
    implicitWidth: Math.min(screen ? screen.width * config.launcherWidthRatio : config.launcherMaximumWidth,
                            config.launcherMaximumWidth)
    implicitHeight: Math.min(screen ? screen.height * config.launcherHeightRatio : config.launcherMaximumHeight,
                             config.launcherMaximumHeight)
    focusable: true
    color: "transparent"

    anchors {
        top: true
        left: true
    }

    // qmllint disable unqualified
    // qmllint disable unresolved-type
    margins.top: Math.round(((root.screen ? root.screen.height : root.implicitHeight) - root.implicitHeight) / 2)
    margins.left: Math.round(((root.screen ? root.screen.width : root.implicitWidth) - root.implicitWidth) / 2)
    // qmllint enable unresolved-type
    // qmllint enable unqualified

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive

    onVisibleChanged: {
        if (visible)
            Qt.callLater(() => searchInput.forceActiveFocus())
    }

    Rectangle {
        anchors.fill: parent
        radius: 10
        color: root.config.notificationBackgroundColor
        border.width: 2
        border.color: root.config.accentColor

        Column {
            anchors {
                fill: parent
                margins: 14
            }
            spacing: 10

            Rectangle {
                width: parent.width
                height: root.config.launcherRowHeight
                radius: 7
                color: root.config.activeBackgroundColor

                TextInput {
                    id: searchInput

                    anchors {
                        fill: parent
                        leftMargin: 12
                        rightMargin: 12
                    }
                    verticalAlignment: TextInput.AlignVCenter
                    color: root.config.textColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.launcherTitleFontPixelSize
                    selectByMouse: true
                    text: root.controller.query
                    onTextChanged: root.controller.setQuery(text)
                    onAccepted: root.controller.accept()

                    Keys.onPressed: event => {
                        if (event.key === Qt.Key_Escape) {
                            if (!root.controller.collapseActions())
                                root.controller.close()
                            event.accepted = true
                        } else if (event.key === Qt.Key_Down
                                || (event.key === Qt.Key_J && event.modifiers & Qt.ControlModifier)) {
                            root.controller.move(1)
                            event.accepted = true
                        } else if (event.key === Qt.Key_Up
                                || (event.key === Qt.Key_K && event.modifiers & Qt.ControlModifier)) {
                            root.controller.move(-1)
                            event.accepted = true
                        } else if (event.key === Qt.Key_PageDown) {
                            root.controller.move(8)
                            event.accepted = true
                        } else if (event.key === Qt.Key_PageUp) {
                            root.controller.move(-8)
                            event.accepted = true
                        } else if (event.key === Qt.Key_Tab
                                || (event.key === Qt.Key_L && event.modifiers & Qt.ControlModifier)) {
                            root.controller.toggleActions()
                            event.accepted = true
                        } else if (event.key === Qt.Key_H && event.modifiers & Qt.ControlModifier) {
                            root.controller.collapseActions()
                            event.accepted = true
                        }
                    }
                }
            }

            ListView {
                id: resultList

                width: parent.width
                height: parent.height - y
                clip: true
                model: root.controller.results
                currentIndex: root.controller.currentIndex
                spacing: 2

                onCurrentIndexChanged: {
                    if (currentIndex >= 0)
                        positionViewAtIndex(currentIndex, ListView.Contain)
                }

                delegate: Rectangle {
                    id: resultRow

                    required property int index
                    required property var modelData
                    width: resultList.width
                    height: modelData.kind === "action"
                        ? root.config.launcherActionRowHeight : root.config.launcherRowHeight
                    radius: 7
                    color: resultRow.index === root.controller.currentIndex
                        ? root.config.activeBackgroundColor : "transparent"

                    Row {
                        anchors {
                            fill: parent
                            leftMargin: resultRow.modelData.kind === "action" ? 46 : 10
                            rightMargin: 10
                        }
                        spacing: 10

                        IconImage {
                            visible: (resultRow.modelData.kind === "application"
                                    && resultRow.modelData.entry.iconSource.length > 0)
                                || (resultRow.modelData.kind === "action"
                                    && root.controller.resolvedIcon(resultRow.modelData.action.icon).length > 0)
                            source: resultRow.modelData.kind === "application"
                                ? resultRow.modelData.entry.iconSource
                                : root.controller.resolvedIcon(resultRow.modelData.action.icon)
                            implicitSize: resultRow.modelData.kind === "action" ? 16 : 24
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Text {
                            visible: resultRow.modelData.kind === "action"
                                && root.controller.resolvedIcon(resultRow.modelData.action.icon).length === 0
                            width: 16
                            text: "↳"
                            color: root.config.accentColor
                            font.family: root.config.fontFamily
                            font.pixelSize: root.config.launcherSubtitleFontPixelSize
                            horizontalAlignment: Text.AlignHCenter
                            anchors.verticalCenter: parent.verticalCenter
                        }

                        Column {
                            width: parent.width - (resultRow.modelData.kind === "application"
                                ? (resultRow.modelData.entry.actions.length > 0 ? 64 : 34) : 26)
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 1

                            Text {
                                width: parent.width
                                text: resultRow.modelData.kind === "action"
                                    ? resultRow.modelData.action.name : resultRow.modelData.entry.title
                                elide: Text.ElideRight
                                color: root.config.textColor
                                font.family: root.config.fontFamily
                                font.pixelSize: resultRow.modelData.kind === "action"
                                    ? root.config.launcherSubtitleFontPixelSize
                                    : root.config.launcherTitleFontPixelSize
                            }

                            Text {
                                visible: resultRow.modelData.kind === "application"
                                    && resultRow.modelData.entry.subtitle.length > 0
                                width: parent.width
                                text: resultRow.modelData.kind === "application"
                                    ? resultRow.modelData.entry.subtitle : ""
                                elide: Text.ElideRight
                                color: root.config.textColor
                                opacity: 0.7
                                font.family: root.config.fontFamily
                                font.pixelSize: root.config.launcherSubtitleFontPixelSize
                            }
                        }

                        Text {
                            visible: resultRow.modelData.kind === "application"
                                && resultRow.modelData.entry.actions.length > 0
                            width: 20
                            text: root.controller.expandedEntryId === resultRow.modelData.entry.id ? "-" : "+"
                            color: root.config.textColor
                            opacity: 0.8
                            font.family: root.config.fontFamily
                            font.pixelSize: root.config.launcherTitleFontPixelSize
                            horizontalAlignment: Text.AlignHCenter
                            anchors.verticalCenter: parent.verticalCenter
                        }
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: {
                            root.controller.select(resultRow.index)
                            root.controller.accept()
                        }
                    }
                }

                Text {
                    anchors.centerIn: parent
                    visible: resultList.count === 0
                    text: "No applications found"
                    color: root.config.textColor
                    opacity: 0.7
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.launcherSubtitleFontPixelSize
                }
            }
        }
    }
}
