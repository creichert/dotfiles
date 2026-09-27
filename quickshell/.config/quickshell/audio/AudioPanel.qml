pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls as Controls
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var audio
    required property var theme

    implicitWidth: 420
    implicitHeight: 440
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
                panelFocus.forceActiveFocus()
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
            KeyNavigation.tab: outputMute
            KeyNavigation.backtab: microphoneSlider.enabled ? microphoneSlider : outputSelector

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
                font.pixelSize: root.theme.titleFontPixelSize + 2
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
                        spacing: root.theme.spacingMedium + 2

                        Text {
                            text: "Output"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize + 2
                            font.bold: true
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Text {
                                Layout.fillWidth: true
                                Layout.minimumWidth: 0
                                text: root.audio.sink
                                    ? (root.audio.sink.description || root.audio.sink.nickname || root.audio.sink.name)
                                    : "No output available"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                                elide: Text.ElideRight
                            }

                            Text {
                                text: root.audio.sink ? `${Math.round(root.audio.sinkVolume * 100)}%` : "--%"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Components.IconButton {
                                id: outputMute

                                theme: root.theme
                                text: root.audio.sinkMuted ? "Unmute output" : "Mute output"
                                iconName: root.audio.sinkMuted ? "volumeMuted"
                                    : root.audio.sinkVolume === 0 ? "volumeOff"
                                    : root.audio.sinkVolume * 100 < root.theme.config.volumeMediumThreshold
                                        ? "volumeLow" : "volumeHigh"
                                iconPixelSize: root.theme.bodyFontPixelSize + 6
                                implicitWidth: 38
                                implicitHeight: 38
                                enabled: root.audio.sink && root.audio.sink.ready
                                KeyNavigation.tab: outputSlider
                                KeyNavigation.backtab: microphoneSlider.enabled ? microphoneSlider : outputSelector
                                onClicked: root.audio.setSinkMuted(!root.audio.sinkMuted)
                            }
                        }

                        Components.Slider {
                            id: outputSlider

                            width: parent.width
                            theme: root.theme
                            handleSize: 20
                            enabled: root.audio.sink && root.audio.sink.ready
                            KeyNavigation.tab: outputSelector
                            KeyNavigation.backtab: outputMute
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
                            text: "Preferred output"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                        }

                        Controls.ComboBox {
                            id: outputSelector

                            width: parent.width
                            height: 42
                            model: root.audio.outputDevices.map(device => ({
                                label: device.description || device.nickname || device.name
                            }))
                            textRole: "label"
                            enabled: root.audio.outputDevices.length > 0
                            hoverEnabled: true
                            focusPolicy: Qt.StrongFocus
                            KeyNavigation.tab: microphoneMute.enabled ? microphoneMute : outputMute
                            KeyNavigation.backtab: outputSlider
                            displayText: currentIndex >= 0 ? currentText
                                : root.audio.outputDevices.length ? "Automatic output" : "No output available"
                            onActivated: index => root.audio.selectOutputDevice(root.audio.outputDevices[index])

                            Binding {
                                target: outputSelector
                                property: "currentIndex"
                                value: root.audio.outputDevices.indexOf(root.audio.preferredSink)
                                when: !outputSelector.popup.visible
                            }

                            contentItem: Text {
                                text: outputSelector.displayText
                                color: outputSelector.enabled ? root.theme.primaryText : root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize + 1
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
                                font.pixelSize: root.theme.bodyFontPixelSize + 2
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
                                height: 42
                                text: outputSelector.textAt(index)
                                hoverEnabled: true
                                contentItem: Text {
                                    text: deviceOption.text
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize + 1
                                    verticalAlignment: Text.AlignVCenter
                                    leftPadding: 10
                                    elide: Text.ElideRight
                                }
                                background: Rectangle {
                                    color: deviceOption.highlighted || deviceOption.hovered
                                        ? root.theme.selectedSurface : root.theme.raisedSurface
                                }
                            }

                            popup.height: Math.min(outputSelector.count * 42 + 8, 176)
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
                        spacing: root.theme.spacingMedium + 2

                        Text {
                            text: "Microphone"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize + 2
                            font.bold: true
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Text {
                                Layout.fillWidth: true
                                Layout.minimumWidth: 0
                                text: !root.audio.source || !root.audio.source.ready
                                    ? "No microphone available"
                                    : (root.audio.source.description || root.audio.source.nickname || root.audio.source.name)
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                                elide: Text.ElideRight
                            }

                            Text {
                                text: root.audio.source && root.audio.source.ready
                                    ? `${Math.round(root.audio.sourceVolume * 100)}%` : "--%"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Components.IconButton {
                                id: microphoneMute

                                theme: root.theme
                                text: root.audio.sourceMuted ? "Unmute microphone" : "Mute microphone"
                                iconName: root.audio.sourceMuted ? "microphoneMuted" : "microphone"
                                iconPixelSize: root.theme.bodyFontPixelSize + 6
                                implicitWidth: 38
                                implicitHeight: 38
                                enabled: root.audio.source && root.audio.source.ready
                                KeyNavigation.tab: microphoneSlider
                                KeyNavigation.backtab: outputSelector
                                onClicked: root.audio.setSourceMuted(!root.audio.sourceMuted)
                            }
                        }

                        Components.Slider {
                            id: microphoneSlider

                            width: parent.width
                            theme: root.theme
                            handleSize: 20
                            enabled: root.audio.source && root.audio.source.ready
                            KeyNavigation.tab: outputMute
                            KeyNavigation.backtab: microphoneMute
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
