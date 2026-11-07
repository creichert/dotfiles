pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var battery
    required property var theme

    implicitWidth: Math.min(340, availableWidth)
    implicitHeight: Math.min(desiredHeight, availableHeight)
    readonly property int desiredHeight: Math.ceil(sections.implicitHeight + theme.panelPadding * 2)
    gap: theme.spacingMedium
    visible: battery.panelVisible
    onDismissed: battery.closePanel()

    onVisibleChanged: {
        if (!visible) {
            if (battery.panelVisible)
                battery.closePanel()
        }
    }

    function duration(seconds) {
        if (!Number.isFinite(seconds) || seconds <= 0)
            return "Estimate unavailable"
        if (seconds < 60)
            return "Less than a minute"
        const minutes = Math.floor(seconds / 60)
        const hours = Math.floor(minutes / 60)
        return hours > 0 ? `${hours} h ${minutes % 60} min` : `${minutes} min`
    }

    Components.PanelBody {
        panel: root
        theme: root.theme

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
                    text: "Battery"
                    color: root.theme.primaryText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.titleFontPixelSize + 2
                    font.bold: true
                }

                RowLayout {
                    width: parent.width
                    spacing: root.theme.spacingLarge

                    Components.Icon {
                        name: root.battery.iconName
                        theme: root.theme
                        font.pixelSize: root.theme.bodyFontPixelSize + 16
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        Layout.minimumWidth: 0
                        spacing: root.theme.spacingSmall

                        Text {
                            visible: root.battery.percent !== null
                            text: root.battery.percent !== null ? `${Math.round(root.battery.percent)}%` : ""
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.titleFontPixelSize + 6
                            font.bold: true
                        }

                        Text {
                            Layout.fillWidth: true
                            text: root.battery.statusText
                            color: root.theme.mutedText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize
                            wrapMode: Text.WordWrap
                        }
                    }
                }

                Column {
                    width: parent.width
                    spacing: root.theme.spacingLarge

                    DetailRow {
                        visible: root.battery.chargeState === "charging" || root.battery.chargeState === "discharging"
                        label: root.battery.chargeState === "charging" ? "Estimated time to full" : "Estimated time to empty"
                        value: root.duration(root.battery.timeEstimateSeconds)
                    }

                    DetailRow {
                        visible: root.battery.rateWatts !== null
                        label: root.battery.chargeState === "charging" ? "Charge rate" : "Discharge rate"
                        value: root.battery.rateWatts !== null ? `${root.battery.rateWatts.toFixed(1)} W` : ""
                    }

                    DetailRow {
                        visible: root.battery.healthPercent !== null
                        label: "Battery health"
                        value: root.battery.healthPercent !== null ? `${root.battery.healthPercent.toFixed(1)}%` : ""
                    }

                    DetailRow {
                        visible: root.battery.fullCapacityWh !== null
                        label: "Full-charge capacity"
                        value: root.battery.fullCapacityWh !== null ? `${root.battery.fullCapacityWh.toFixed(1)} Wh` : ""
                    }
                }
            }
        }
    }

    component DetailRow: RowLayout {
        id: row

        required property string label
        required property string value
        width: parent.width
        spacing: root.theme.spacingMedium

        Text {
            Layout.fillWidth: true
            Layout.minimumWidth: 0
            text: row.label
            color: root.theme.mutedText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.bodyFontPixelSize
            wrapMode: Text.WordWrap
        }

        Text {
            Layout.maximumWidth: row.width * 0.55
            text: row.value
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.bodyFontPixelSize
            wrapMode: Text.WordWrap
            horizontalAlignment: Text.AlignRight
        }
    }
}
