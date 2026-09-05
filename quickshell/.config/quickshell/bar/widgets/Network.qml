import QtQuick

Item {
    id: root

    required property var metrics
    required property var config
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
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }

    Row {
        id: networkRow
        anchors.centerIn: parent
        spacing: root.config.networkSpacing

        Text {
            text: root.metrics.interfaceName.length > 0 ? root.metrics.interfaceName : "Disconnected"
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }

        Text {
            text: root.metrics.interfaceName.length > 0 ? "󰱔 |" : "⚠"
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.transmitBytesPerSecond) : ""
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }

        Text {
            text: ""
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.receiveBytesPerSecond) : ""
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }

        Text {
            text: ""
            color: root.config.textColor
            font.family: root.config.fontFamily
            font.pixelSize: root.config.fontPixelSize
        }
    }
}
