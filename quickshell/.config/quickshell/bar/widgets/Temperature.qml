import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var metrics
    required property var config
    required property var theme
    implicitWidth: temperatureRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function icon() {
        if (metrics.temperatureC >= config.temperatureCriticalThreshold)
            return "temperatureCritical"
        if (metrics.temperatureC < config.temperatureCoolThreshold)
            return "temperatureCool"
        if (metrics.temperatureC < config.temperatureWarmThreshold)
            return "temperatureWarm"
        return "temperatureHot"
    }

    Row {
        id: temperatureRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: `${root.metrics.temperatureC}°C `
            color: root.metrics.temperatureC >= root.config.temperatureCriticalThreshold
                ? root.theme.urgent : root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: root.icon()
            theme: root.theme
            color: root.metrics.temperatureC >= root.config.temperatureCriticalThreshold
                ? root.theme.urgent : root.theme.primaryText
        }
    }
}
