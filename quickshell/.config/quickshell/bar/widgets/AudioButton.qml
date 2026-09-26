import QtQuick
import "../../components" as Components

Item {
    id: root

    required property var config
    required property var theme
    required property var audio
    implicitWidth: volumeRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    readonly property var sink: audio.sink
    readonly property bool muted: audio.sinkMuted
    readonly property int percent: Math.round(audio.sinkVolume * 100)

    function icon() {
        if (percent === 0)
            return "volumeOff"
        if (percent < config.volumeMediumThreshold)
            return "volumeLow"
        return "volumeHigh"
    }

    Row {
        id: volumeRow
        anchors.centerIn: parent
        spacing: 0

        Text {
            visible: !root.muted || !root.sink
            text: !root.sink ? "--% " : `${root.percent}% `
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: !root.sink ? "volumeHigh" : root.muted ? "volumeMuted" : root.icon()
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: root.audio.toggleSinkMuted()
        onWheel: wheel => root.audio.adjustSinkVolume(wheel.angleDelta.y > 0 ? 0.03 : -0.03)
    }
}
