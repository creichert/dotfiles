pragma ComponentBehavior: Bound

// qmllint disable uncreatable-type

import QtQuick
import Quickshell
import Quickshell.Wayland

PanelWindow {
    id: root

    required property var config
    required property var controller
    screen: controller.targetScreen
    implicitWidth: Math.min(screen ? screen.width * config.clipboardWidthRatio : config.clipboardMaximumWidth,
                            config.clipboardMaximumWidth)
    implicitHeight: Math.min(screen ? screen.height * config.clipboardHeightRatio : config.clipboardMaximumHeight,
                             config.clipboardMaximumHeight)
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
        radius: root.config.surfaceRadius
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
                radius: root.config.controlRadius
                color: root.config.activeBackgroundColor

                Text {
                    anchors {
                        fill: parent
                        leftMargin: 12
                        rightMargin: 12
                    }
                    visible: searchInput.text.length === 0
                    verticalAlignment: Text.AlignVCenter
                    text: "Search clipboard history..."
                    color: root.config.textMutedColor
                    font.family: root.config.fontFamily
                    font.pixelSize: root.config.launcherTitleFontPixelSize
                }

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
                        }
                    }
                }
            }

            Row {
                width: parent.width
                height: parent.height - y
                spacing: 10

                ListView {
                    id: resultList

                    width: Math.round(parent.width * root.config.clipboardListWidthRatio)
                    height: parent.height
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
                        height: root.config.clipboardRowHeight
                        radius: root.config.controlRadius
                        color: resultRow.index === root.controller.currentIndex
                            ? root.config.activeBackgroundColor : "transparent"

                        Row {
                            anchors {
                                fill: parent
                                leftMargin: 10
                                rightMargin: 10
                            }
                            spacing: 10

                            Text {
                                width: 22
                                anchors.verticalCenter: parent.verticalCenter
                                text: resultRow.modelData.kind === "image" ? ""
                                    : resultRow.modelData.kind === "unsupported" ? "" : ""
                                color: root.config.accentColor
                                horizontalAlignment: Text.AlignHCenter
                                font.family: root.config.fontFamily
                                font.pixelSize: root.config.launcherTitleFontPixelSize
                            }

                            Column {
                                width: parent.width - 32
                                anchors.verticalCenter: parent.verticalCenter
                                spacing: 2

                                Text {
                                    width: parent.width
                                    text: resultRow.modelData.title
                                    color: root.config.textColor
                                    elide: Text.ElideRight
                                    maximumLineCount: 2
                                    wrapMode: Text.Wrap
                                    font.family: root.config.fontFamily
                                    font.pixelSize: root.config.launcherSubtitleFontPixelSize
                                }

                                Text {
                                    width: parent.width
                                    text: resultRow.modelData.details
                                    color: root.config.textMutedColor
                                    elide: Text.ElideRight
                                    font.family: root.config.fontFamily
                                    font.pixelSize: root.config.launcherSubtitleFontPixelSize - 2
                                }
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
                        width: parent.width - 24
                        text: {
                            if (root.controller.loading)
                                return "Loading clipboard history..."
                            if (root.controller.errorMessage.length > 0)
                                return root.controller.errorMessage
                            return root.controller.query.length > 0
                                ? "No matching clipboard entries" : "Clipboard history is empty"
                        }
                        color: root.config.textMutedColor
                        horizontalAlignment: Text.AlignHCenter
                        wrapMode: Text.Wrap
                        font.family: root.config.fontFamily
                        font.pixelSize: root.config.launcherSubtitleFontPixelSize
                    }
                }

                Rectangle {
                    width: parent.width - resultList.width - parent.spacing
                    height: parent.height
                    radius: root.config.controlRadius
                    color: root.config.surfaceBaseColor
                    border.width: 1
                    border.color: root.config.separatorColor

                    Column {
                        anchors {
                            fill: parent
                            margins: 12
                        }
                        spacing: 8

                        Row {
                            width: parent.width
                            height: 22

                            Text {
                                width: parent.width
                                text: root.controller.currentEntry
                                    ? root.controller.currentEntry.details : "Preview"
                                color: root.config.textMutedColor
                                elide: Text.ElideRight
                                font.family: root.config.fontFamily
                                font.pixelSize: root.config.launcherSubtitleFontPixelSize
                            }
                        }

                        Rectangle {
                            width: parent.width
                            height: 1
                            color: root.config.separatorColor
                        }

                        Item {
                            width: parent.width
                            height: parent.height - y

                            Flickable {
                                id: textPreview

                                anchors.fill: parent
                                visible: root.controller.currentEntry
                                    && root.controller.currentEntry.kind === "text"
                                    && !root.controller.previewLoading
                                contentWidth: width
                                contentHeight: previewText.implicitHeight
                                clip: true

                                Text {
                                    id: previewText

                                    width: textPreview.width
                                    text: root.controller.previewText
                                        + (root.controller.previewTruncated
                                            ? "\n\n[Preview truncated after "
                                                + root.config.clipboardPreviewMaximumCharacters
                                                + " characters]" : "")
                                    color: root.config.textColor
                                    textFormat: Text.PlainText
                                    wrapMode: Text.Wrap
                                    font.family: root.config.fontFamily
                                    font.pixelSize: root.config.launcherSubtitleFontPixelSize
                                }
                            }

                            Image {
                                id: imagePreview

                                anchors.fill: parent
                                visible: root.controller.currentEntry
                                    && root.controller.currentEntry.kind === "image"
                                    && !root.controller.previewLoading
                                source: root.controller.previewImageSource
                                sourceSize.width: width
                                sourceSize.height: height
                                fillMode: Image.PreserveAspectFit
                                asynchronous: true
                                cache: false
                                autoTransform: true
                            }

                            Text {
                                anchors.centerIn: parent
                                visible: root.controller.previewLoading
                                text: "Loading preview..."
                                color: root.config.textMutedColor
                                font.family: root.config.fontFamily
                                font.pixelSize: root.config.launcherSubtitleFontPixelSize
                            }

                            Text {
                                anchors.centerIn: parent
                                visible: !root.controller.previewLoading
                                    && (root.controller.currentEntry === null
                                        || (root.controller.errorMessage.length > 0
                                            && root.controller.previewText.length === 0
                                            && root.controller.previewImageSource.length === 0)
                                        || imagePreview.status === Image.Error)
                                width: parent.width - 24
                                text: root.controller.errorMessage.length > 0
                                    ? root.controller.errorMessage : "Select an entry to preview"
                                color: root.config.textMutedColor
                                horizontalAlignment: Text.AlignHCenter
                                wrapMode: Text.Wrap
                                font.family: root.config.fontFamily
                                font.pixelSize: root.config.launcherSubtitleFontPixelSize
                            }
                        }
                    }
                }
            }
        }
    }
}
