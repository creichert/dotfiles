pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var resources
    required property var theme

    implicitWidth: screen ? Math.min(380, Math.max(1, screen.width - 2 * edgeInset)) : 380
    implicitHeight: screen ? Math.min(desiredHeight, Math.max(1,
        screen.height - triggerRect.y - triggerRect.height - gap - edgeInset)) : desiredHeight
    readonly property int desiredHeight: Math.ceil(sections.implicitHeight + theme.panelPadding * 2)
    gap: theme.spacingMedium
    visible: resources.panelVisible
    onDismissed: resources.closePanel()

    onVisibleChanged: {
        if (!visible) {
            if (resources.panelVisible)
                resources.closePanel()
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

            Keys.onEscapePressed: event => {
                root.resources.closePanel()
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
                        text: "System Resources"
                        color: root.theme.primaryText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.titleFontPixelSize + 2
                        font.bold: true
                    }

                    Text {
                        visible: !root.resources.sampleAvailable
                        text: "Waiting for resource data"
                        color: root.theme.mutedText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.secondaryFontPixelSize
                    }

                    ResourceRow {
                        width: parent.width
                        theme: root.theme
                        iconName: root.resources.cpuIcon
                        label: "CPU"
                        value: root.resources.cpuAvailable ? `${root.resources.cpuPercent}%` : "--%"
                        detail: root.resources.cpuStatusText
                    }

                    ResourceRow {
                        width: parent.width
                        theme: root.theme
                        iconName: "memory"
                        label: "Memory"
                        value: root.resources.memoryAvailable ? `${root.resources.memoryPercent}%` : "--%"
                        detail: root.resources.memoryCapacityText
                    }

                    Rectangle {
                        visible: root.resources.temperatureSupported
                        width: parent.width
                        height: 1
                        color: root.theme.separator
                    }

                    ResourceRow {
                        visible: root.resources.temperatureSupported
                        width: parent.width
                        theme: root.theme
                        iconName: root.resources.temperatureIcon
                        label: "CPU temperature"
                        value: root.resources.temperatureAvailable ? `${root.resources.temperatureC}°C` : "--°C"
                        detail: `${root.resources.temperatureStatusText} · ${root.resources.temperatureSensorText}`
                        urgent: root.resources.temperatureCritical
                    }

                    Text {
                        width: parent.width
                        text: root.resources.cadenceText
                        color: root.theme.mutedText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.secondaryFontPixelSize
                        wrapMode: Text.WordWrap
                    }
                }
            }
        }
    }

    component ResourceRow: RowLayout {
        id: row

        required property var theme
        required property string iconName
        required property string label
        required property string value
        required property string detail
        property bool urgent: false
        spacing: theme.spacingLarge

        Components.Icon {
            Layout.alignment: Qt.AlignTop
            Layout.preferredWidth: 28
            horizontalAlignment: Text.AlignHCenter
            name: row.iconName
            theme: row.theme
            font.pixelSize: row.theme.bodyFontPixelSize + 8
            color: row.urgent ? row.theme.urgent : row.theme.primaryText
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.minimumWidth: 0
            spacing: row.theme.spacingSmall

            RowLayout {
                Layout.fillWidth: true
                spacing: row.theme.spacingMedium

                Text {
                    Layout.fillWidth: true
                    Layout.minimumWidth: 0
                    text: row.label
                    color: row.theme.primaryText
                    font.family: row.theme.fontFamily
                    font.pixelSize: row.theme.bodyFontPixelSize + 2
                    font.bold: true
                    elide: Text.ElideRight
                }

                Text {
                    text: row.value
                    color: row.urgent ? row.theme.urgent : row.theme.primaryText
                    font.family: row.theme.fontFamily
                    font.pixelSize: row.theme.titleFontPixelSize + 2
                    font.bold: true
                }
            }

            Text {
                Layout.fillWidth: true
                text: row.detail
                color: row.theme.mutedText
                font.family: row.theme.fontFamily
                font.pixelSize: row.theme.secondaryFontPixelSize
                wrapMode: Text.WordWrap
            }
        }
    }
}
