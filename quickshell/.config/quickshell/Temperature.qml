import QtQuick

Item {
    id: root

    required property var metrics
    implicitWidth: temperatureText.implicitWidth + 16
    implicitHeight: 30

    function icon() {
        if (metrics.temperatureC >= 85)
            return ""
        if (metrics.temperatureC < 50)
            return ""
        if (metrics.temperatureC < 70)
            return ""
        return ""
    }

    Text {
        id: temperatureText
        anchors.centerIn: parent
        text: `${parent.metrics.temperatureC}°C ${parent.icon()}`
        color: parent.metrics.temperatureC >= 85 ? "#eb4d4b" : "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
    }
}
