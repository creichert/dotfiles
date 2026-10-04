pragma ComponentBehavior: Bound

import QtQuick
import "../../components" as Components

Item {
    id: root

    // QQuickItem already owns the resources list property.
    required property var resourcesService
    required property var config
    required property var theme
    property bool expanded: false
    implicitWidth: resourceRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    readonly property string compactResource: resourcesService.temperatureCritical ? "temperature"
        : resourcesService.memoryUrgent ? "memory" : "cpu"
    readonly property real valueWidth: Math.max(percentMetrics.width, temperatureMetrics.width)

    TextMetrics {
        id: percentMetrics
        text: "100%"
        font.family: root.theme.fontFamily
        font.pixelSize: root.theme.fontPixelSize
    }

    TextMetrics {
        id: temperatureMetrics
        text: "100°"
        font.family: root.theme.fontFamily
        font.pixelSize: root.theme.fontPixelSize
    }

    Row {
        id: resourceRow
        anchors.centerIn: parent
        spacing: root.theme.spacingMedium

        Headline {
            visible: root.expanded || root.compactResource === "cpu"
            value: root.resourcesService.cpuAvailable ? `${root.resourcesService.cpuPercent}%` : "--%"
            iconName: "cpu"
        }

        Headline {
            visible: root.expanded || root.compactResource === "memory"
            value: root.resourcesService.memoryAvailable ? `${root.resourcesService.memoryPercent}%` : "--%"
            iconName: "memory"
            urgent: root.resourcesService.memoryUrgent
        }

        Headline {
            visible: root.expanded || root.compactResource === "temperature"
            value: root.resourcesService.temperatureAvailable ? `${root.resourcesService.temperatureC}°` : "--°"
            iconName: root.resourcesService.temperatureIcon
            urgent: root.resourcesService.temperatureCritical
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton)
                root.resourcesService.togglePanel()
            else if (mouse.button === Qt.RightButton)
                root.expanded = !root.expanded
        }
    }

    // Metric formatting is private to this Resources experiment.
    component Headline: Row {
        id: headline

        required property string value
        required property string iconName
        property bool urgent: false
        spacing: root.theme.spacingSmall

        Text {
            width: Math.max(root.valueWidth, implicitWidth)
            horizontalAlignment: Text.AlignRight
            text: headline.value
            color: headline.urgent ? root.theme.urgent : root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            width: Math.max(root.theme.fontPixelSize, implicitWidth)
            horizontalAlignment: Text.AlignHCenter
            name: headline.iconName
            theme: root.theme
            color: headline.urgent ? root.theme.urgent : root.theme.primaryText
        }
    }
}
