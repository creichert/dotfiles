pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls as Controls
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var audio
    required property var theme

    implicitWidth: 380
    implicitHeight: 420
    gap: theme.spacingMedium
    visible: audio.panelVisible
    onDismissed: audio.closePanel()

    onVisibleChanged: {
        if (!visible) {
            if (audio.panelVisible)
                audio.closePanel()
            return
        }

        Qt.callLater(() => {
            if (root.visible)
                outputSlider.enabled ? outputSlider.forceActiveFocus() : panelFocus.forceActiveFocus()
        })
    }

    Rectangle {
        anchors.fill: parent
        radius: root.theme.surfaceRadius
        color: root.theme.raisedSurface
        border.width: 1
        border.color: root.theme.separator

        // Keep clicks on empty panel space from dismissing the native popup.
        MouseArea {
            anchors.fill: parent
        }

        FocusScope {
            id: panelFocus

            anchors {
                fill: parent
                margins: root.theme.panelPadding
            }
            focus: true

            Keys.onEscapePressed: event => {
                root.audio.closePanel()
                event.accepted = true
            }

            Text {
                id: title

                anchors.top: parent.top
                text: "Audio"
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.titleFontPixelSize
                font.bold: true
            }

            Flickable {
                id: contents

                anchors {
                    top: title.bottom
                    topMargin: root.theme.sectionSpacing
                    left: parent.left
                    right: parent.right
                    bottom: parent.bottom
                }
                contentWidth: width
                contentHeight: sections.implicitHeight
                clip: true
                interactive: contentHeight > height

                Column {
                    id: sections

                    width: contents.width
                    spacing: root.theme.sectionSpacing

                    Column {
                        width: parent.width
                        spacing: root.theme.spacingMedium

                        Text {
                            text: "Output"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                            font.bold: true
                        }

                        Text {
                            width: parent.width
                            text: root.audio.sink
                                ? (root.audio.sink.description || root.audio.sink.nickname || root.audio.sink.name)
                                : "No output available"
                            color: root.theme.mutedText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.secondaryFontPixelSize
                            elide: Text.ElideRight
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Text {
                                text: "Volume"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Item { Layout.fillWidth: true }

                            Text {
                                text: root.audio.sink ? `${Math.round(root.audio.sinkVolume * 100)}%` : "--%"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Components.IconButton {
                                theme: root.theme
                                text: root.audio.sinkMuted ? "Unmute output" : "Mute output"
                                iconName: root.audio.sinkMuted ? "volumeHigh" : "volumeMuted"
                                enabled: root.audio.sink && root.audio.sink.ready
                                onClicked: root.audio.setSinkMuted(!root.audio.sinkMuted)
                            }
                        }

                        Components.Slider {
                            id: outputSlider

                            width: parent.width
                            theme: root.theme
                            enabled: root.audio.sink && root.audio.sink.ready
                            onMoved: root.audio.setSinkVolume(value)

                            Binding {
                                target: outputSlider
                                property: "value"
                                value: root.audio.sinkVolume
                                when: !outputSlider.pressed
                                restoreMode: Binding.RestoreNone
                            }
                        }

                        Text {
                            text: "Output device"
                            color: root.theme.mutedText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.secondaryFontPixelSize
                        }

                        Controls.ComboBox {
                            id: outputSelector

                            width: parent.width
                            height: 36
                            model: root.audio.outputDevices.map(device => ({
                                label: device.description || device.nickname || device.name
                            }))
                            textRole: "label"
                            enabled: root.audio.outputDevices.length > 0
                            hoverEnabled: true
                            focusPolicy: Qt.StrongFocus
                            displayText: currentIndex < 0 ? "No output selected" : currentText
                            onActivated: index => root.audio.selectOutputDevice(root.audio.outputDevices[index])

                            Binding {
                                target: outputSelector
                                property: "currentIndex"
                                value: root.audio.outputDevices.indexOf(root.audio.sink)
                                when: !outputSelector.popup.visible
                            }

                            contentItem: Text {
                                text: outputSelector.displayText
                                color: outputSelector.enabled ? root.theme.primaryText : root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                                verticalAlignment: Text.AlignVCenter
                                leftPadding: 10
                                rightPadding: 28
                                elide: Text.ElideRight
                            }

                            indicator: Text {
                                x: outputSelector.width - width - 10
                                y: (outputSelector.height - height) / 2
                                text: "⌄"
                                color: root.theme.mutedText
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            background: Rectangle {
                                radius: root.theme.controlRadius
                                color: outputSelector.down ? root.theme.selectedSurface
                                    : outputSelector.hovered ? root.theme.surface : root.theme.raisedSurface
                                border.width: 1
                                border.color: outputSelector.visualFocus ? root.theme.activeAccent
                                    : outputSelector.hovered ? root.theme.mutedText : root.theme.separator
                            }

                            delegate: Controls.ItemDelegate {
                                id: deviceOption

                                required property int index

                                width: outputSelector.width
                                height: 36
                                text: outputSelector.textAt(index)
                                hoverEnabled: true
                                contentItem: Text {
                                    text: deviceOption.text
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                    verticalAlignment: Text.AlignVCenter
                                    leftPadding: 10
                                    elide: Text.ElideRight
                                }
                                background: Rectangle {
                                    color: deviceOption.highlighted || deviceOption.hovered
                                        ? root.theme.selectedSurface : root.theme.raisedSurface
                                }
                            }

                            popup.height: Math.min(outputSelector.count * 36 + 8, 160)
                            popup.background: Rectangle {
                                color: root.theme.raisedSurface
                                border.width: 1
                                border.color: root.theme.separator
                                radius: root.theme.controlRadius
                            }
                        }
                    }

                    Rectangle {
                        width: parent.width
                        height: 1
                        color: root.theme.separator
                    }

                    Column {
                        width: parent.width
                        spacing: root.theme.spacingMedium

                        Text {
                            text: "Microphone"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                            font.bold: true
                        }

                        Text {
                            width: parent.width
                            text: !root.audio.source || !root.audio.source.ready
                                ? "No microphone available"
                                : root.audio.sourceMuted ? "Muted" : "Available"
                            color: root.theme.mutedText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.secondaryFontPixelSize
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Text {
                                text: "Volume"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Item { Layout.fillWidth: true }

                            Text {
                                text: root.audio.source ? `${Math.round(root.audio.sourceVolume * 100)}%` : "--%"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Components.IconButton {
                                theme: root.theme
                                text: root.audio.sourceMuted ? "Unmute microphone" : "Mute microphone"
                                iconName: root.audio.sourceMuted ? "microphone" : "microphoneMuted"
                                enabled: root.audio.source && root.audio.source.ready
                                onClicked: root.audio.setSourceMuted(!root.audio.sourceMuted)
                            }
                        }

                        Components.Slider {
                            id: microphoneSlider

                            width: parent.width
                            theme: root.theme
                            enabled: root.audio.source && root.audio.source.ready
                            onMoved: root.audio.setSourceVolume(value)

                            Binding {
                                target: microphoneSlider
                                property: "value"
                                value: root.audio.sourceVolume
                                when: !microphoneSlider.pressed
                                restoreMode: Binding.RestoreNone
                            }
                        }
                    }
                }
            }
        }
    }
}
