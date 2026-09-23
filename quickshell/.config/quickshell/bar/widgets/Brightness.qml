import QtQuick

Item {
    id: root

    required property var metrics
    required property var config
    readonly property bool available: metrics.brightnessPercent !== null
    readonly property var icons: ["", "", "", "", "", "", "", "", ""]

    visible: config.brightnessModuleEnabled
    implicitWidth: brightnessText.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    function icon() {
        if (!root.available)
            return root.icons[0]

        const index = Math.min(
            root.icons.length - 1,
            Math.floor(root.metrics.brightnessPercent / 100 * root.icons.length)
        )
        return root.icons[index]
    }

    Text {
        id: brightnessText
        anchors.centerIn: parent
        text: root.available
            ? `${Math.round(root.metrics.brightnessPercent)}% ${root.icon()}`
            : "--"
        color: root.config.textColor
        font.family: root.config.fontFamily
        font.pixelSize: root.config.fontPixelSize
    }
}
