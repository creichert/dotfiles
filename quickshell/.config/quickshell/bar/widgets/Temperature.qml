import QtQuick
import "../../components" as Components

Item {
    id: root

    // QQuickItem already owns the resources list property.
    required property var resourcesService
    required property var config
    required property var theme
    implicitWidth: temperatureRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    Row {
        id: temperatureRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            text: root.resourcesService.temperatureAvailable ? `${root.resourcesService.temperatureC}°C ` : "--°C "
            color: root.resourcesService.temperatureCritical
                ? root.theme.urgent : root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: root.resourcesService.temperatureIcon
            theme: root.theme
            color: root.resourcesService.temperatureCritical
                ? root.theme.urgent : root.theme.primaryText
        }
    }
}
