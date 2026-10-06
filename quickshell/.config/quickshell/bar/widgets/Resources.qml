pragma ComponentBehavior: Bound

import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    // QQuickItem already owns the resources list property.
    required property var resourcesService
    property bool expanded: false
    engaged: resourcesService.panelVisible

    readonly property string compactResource: resourcesService.temperatureCritical ? "temperature"
        : resourcesService.memoryUrgent ? "memory" : "cpu"

    contentItem: Row {
        id: resourceRow
        spacing: 2 * root.config.barContentSpacing

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
        spacing: root.config.barContentSpacing

        Text {
            text: headline.value
            color: headline.urgent ? root.theme.urgent : root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: headline.iconName
            theme: root.theme
            color: headline.urgent ? root.theme.urgent : root.theme.primaryText
        }
    }
}
