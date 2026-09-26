import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var metrics
    required property var config
    required property var theme
    visible: config.networkModuleEnabled
    implicitWidth: networkRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function rate(bytes) {
        const bits = bytes * 8
        if (bits < 1000)
            return `${Math.round(bits)} b/s`
        if (bits < 1000000)
            return `${(bits / 1000).toFixed(1)} Kb/s`
        return `${(bits / 1000000).toFixed(1)} Mb/s`
    }

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
            text: root.metrics.interfaceName.length > 0 ? root.metrics.interfaceName : "Disconnected"
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Row {
            spacing: 0

            Components.Icon {
                name: root.metrics.interfaceName.length > 0
                    ? "networkConnected" : "networkDisconnected"
                theme: root.theme
            }

            Text {
                visible: root.metrics.interfaceName.length > 0
                text: " |"
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.fontPixelSize
            }
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.transmitBytesPerSecond) : ""
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
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.receiveBytesPerSecond) : ""
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: "download"
            theme: root.theme
        }
    }
}
