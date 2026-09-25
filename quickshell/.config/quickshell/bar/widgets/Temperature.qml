import QtQuick

Item {
    id: root

    required property var metrics
    required property var config
    implicitWidth: temperatureText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function icon() {
        if (metrics.temperatureC >= config.temperatureCriticalThreshold)
            return ""
        if (metrics.temperatureC < config.temperatureCoolThreshold)
            return ""
        if (metrics.temperatureC < config.temperatureWarmThreshold)
            return ""
        return ""
    }

    Text {
        id: temperatureText
        anchors.centerIn: parent
        text: `${parent.metrics.temperatureC}°C ${parent.icon()}`
        color: parent.metrics.temperatureC >= parent.config.temperatureCriticalThreshold
            ? parent.config.urgentBackgroundColor
            : parent.config.textColor
        font.family: parent.config.fontFamily
        font.pixelSize: parent.config.fontPixelSize
    }
}
