import QtQuick
import Quickshell.Services.Pipewire
import "../../components" as Components

Item {
    id: root

    required property var config
    required property var theme
    implicitWidth: volumeRow.implicitWidth + config.moduleHorizontalPadding
    implicitHeight: config.barHeight

    readonly property var sink: Pipewire.ready ? Pipewire.defaultAudioSink : null
    readonly property bool muted: sink && sink.audio ? sink.audio.muted : false
    readonly property int percent: sink && sink.audio ? Math.round(sink.audio.volume * 100) : 0

    function icon() {
        if (percent === 0)
            return "volumeOff"
        if (percent < config.volumeMediumThreshold)
            return "volumeLow"
        return "volumeHigh"
    }

    PwObjectTracker {
        objects: [root.sink]
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
        onClicked: {
            if (parent.sink && parent.sink.audio)
                parent.sink.audio.muted = !parent.sink.audio.muted
        }
        onWheel: wheel => {
            if (parent.sink && parent.sink.audio)
                parent.sink.audio.volume = Math.max(0, Math.min(1, parent.sink.audio.volume + (wheel.angleDelta.y > 0 ? 0.03 : -0.03)))
        }
    }
}
