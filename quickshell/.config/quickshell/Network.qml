import QtQuick

Item {
    id: root

    required property var metrics
    implicitWidth: networkRow.implicitWidth + 16
    implicitHeight: 30

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
        text: "999.9 Mb/s"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }

    Row {
        id: networkRow
        anchors.centerIn: parent
        spacing: 6

        Text {
            text: root.metrics.interfaceName.length > 0 ? root.metrics.interfaceName : "Disconnected"
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }

        Text {
            text: root.metrics.interfaceName.length > 0 ? "󰱔 |" : "⚠"
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.transmitBytesPerSecond) : ""
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }

        Text {
            text: ""
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }

        Text {
            width: rateMetrics.width
            horizontalAlignment: Text.AlignRight
            text: root.metrics.interfaceName.length > 0 ? root.rate(root.metrics.receiveBytesPerSecond) : ""
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }

        Text {
            text: ""
            color: "white"
            font.family: "Hack Nerd Font Propo"
            font.pixelSize: 14
        }
    }
}
