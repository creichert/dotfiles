pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var display
    required property var theme

    implicitWidth: Math.min(340, availableWidth)
    implicitHeight: Math.min(Math.ceil(sections.implicitHeight + theme.panelPadding * 2), availableHeight)
    gap: theme.spacingMedium
    visible: display.panelVisible
    onDismissed: display.closePanel()

    onVisibleChanged: {
        if (!visible) {
            if (display.panelVisible)
                display.closePanel()
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
        color: root.theme.surface
        border.width: 1
        border.color: root.theme.separator

        MouseArea {
            anchors.fill: parent
        }

        FocusScope {
            id: panelFocus

            anchors.fill: parent
            anchors.margins: root.theme.panelPadding
            focus: true
            KeyNavigation.tab: brightnessSlider.enabled ? brightnessSlider : null

            Keys.onEscapePressed: event => {
                root.display.closePanel()
                event.accepted = true
            }

            Flickable {
                anchors.fill: parent
                contentWidth: width
                contentHeight: sections.implicitHeight
                clip: true
                interactive: contentHeight > height

                Column {
                    id: sections

                    width: parent.width
                    spacing: root.theme.sectionSpacing

                    Text {
                        text: "Display"
                        color: root.theme.primaryText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.titleFontPixelSize + 2
                        font.bold: true
                    }

                    RowLayout {
                        width: parent.width
                        spacing: root.theme.spacingMedium

                        Components.Icon {
                            name: root.display.brightnessIconName
                            theme: root.theme
                            font.pixelSize: root.theme.bodyFontPixelSize + 8
                        }

                        Text {
                            Layout.fillWidth: true
                            text: "Brightness"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                        }

                        Text {
                            text: root.display.brightnessPercent !== null
                                ? `${Math.round(root.display.brightnessPercent)}%` : "--%"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                        }
                    }

                    Components.Slider {
                        id: brightnessSlider

                        property var pendingPercent: null
                        width: parent.width
                        theme: root.theme
                        from: 0
                        to: 100
                        stepSize: 1
                        enabled: root.display.brightnessAdjustable

                        // Preview drags locally; commit once on release. Native
                        // keyboard/wheel moves commit immediately, without any
                        // writes caused by observed-value bindings.
                        onMoved: {
                            if (pressed)
                                pendingPercent = value
                            else
                                root.display.setBrightnessPercent(value)
                        }
                        onPressedChanged: {
                            if (!pressed && pendingPercent !== null) {
                                const percent = pendingPercent
                                root.display.setBrightnessPercent(percent)
                                pendingPercent = null
                            }
                        }

                        Binding {
                            target: brightnessSlider
                            property: "value"
                            value: root.display.brightnessPendingPercent !== null
                                ? root.display.brightnessPendingPercent
                                : root.display.brightnessPercent !== null ? root.display.brightnessPercent : 0
                            // Register the release request before resuming the
                            // binding; then hold its target until confirmation.
                            when: !brightnessSlider.pressed && brightnessSlider.pendingPercent === null
                            restoreMode: Binding.RestoreNone
                        }
                    }

                    Text {
                        width: parent.width
                        text: root.display.brightnessStatusText
                        color: root.theme.mutedText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.bodyFontPixelSize
                        wrapMode: Text.WordWrap
                    }
                }
            }
        }
    }
}
