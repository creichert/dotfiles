import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var network
    required property var config
    required property var theme
    visible: config.networkModuleEnabled
    implicitWidth: networkRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    TextMetrics {
        id: rateMetrics
        text: root.config.networkRateWidthLabel
        font.family: root.theme.fontFamily
        font.pixelSize: root.theme.fontPixelSize
    }

    Row {
        id: networkRow
        anchors.centerIn: parent
        spacing: root.config.networkSpacing

        Text {
            text: root.network.interfaceName.length > 0 ? root.network.interfaceName : "No route"
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Row {
            spacing: 0

            Components.Icon {
                name: root.network.interfaceName.length > 0 && root.network.linkState !== "down"
                    ? "networkConnected" : "networkDisconnected"
                theme: root.theme
            }

            Text {
                visible: root.network.interfaceName.length > 0
                text: " |"
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.fontPixelSize
            }
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.network.interfaceName.length > 0
                ? root.network.rate(root.network.transmitBytesPerSecond) : ""
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "upload"
            theme: root.theme
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.network.interfaceName.length > 0
                ? root.network.rate(root.network.receiveBytesPerSecond) : ""
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "download"
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.network.togglePanel()
    }
}
