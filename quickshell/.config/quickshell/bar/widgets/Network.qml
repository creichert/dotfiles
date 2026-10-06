import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    required property var network
    visible: config.networkModuleEnabled
    engaged: network.panelVisible
    property bool expanded: false

    readonly property bool hasRoute: network.interfaceName.length > 0
        && network.linkState !== "down"
    readonly property string connectionGlyph: {
        if (!hasRoute)
            return "networkUnavailable"

        if (network.wifiConnected) {
            const signal = network.wifiSignalPercent
            if (signal === null)
                return "wifiStrengthOutline"
            if (signal <= 25)
                return "wifiStrength1"
            if (signal <= 50)
                return "wifiStrength2"
            if (signal <= 75)
                return "wifiStrength3"
            return "wifiStrength4"
        }

        // Experiment-only inference: on the current two hosts, a route-up
        // interface without connected Wi-Fi is presented as wired/Ethernet.
        return "ethernet"
    }

    TextMetrics {
        id: rateMetrics
        text: root.config.networkRateWidthLabel
        font.family: root.theme.fontFamily
        font.pixelSize: root.theme.fontPixelSize
    }

    contentItem: Row {
        id: networkRow
        spacing: root.config.barContentSpacing

        Components.Icon {
            name: root.connectionGlyph
            theme: root.theme
        }

        Row {
            visible: root.expanded && root.hasRoute
            spacing: root.config.barContentSpacing

            Text {
                width: rateMetrics.width
                horizontalAlignment: Text.AlignRight
                text: root.network.rate(root.network.receiveBytesPerSecond)
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.fontPixelSize
            }

            Components.Icon {
                name: "download"
                theme: root.theme
            }

            Text {
                width: rateMetrics.width
                horizontalAlignment: Text.AlignRight
                text: root.network.rate(root.network.transmitBytesPerSecond)
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.fontPixelSize
            }

            Components.Icon {
                name: "upload"
                theme: root.theme
            }

        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton)
                root.network.togglePanel()
            else if (mouse.button === Qt.RightButton)
                root.expanded = !root.expanded
        }
    }
}
