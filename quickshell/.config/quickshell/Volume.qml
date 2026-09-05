import QtQuick
import Quickshell.Services.Pipewire

Item {
    id: root

    implicitWidth: volumeText.implicitWidth + 16
    implicitHeight: 30

    readonly property var sink: Pipewire.ready ? Pipewire.defaultAudioSink : null
    readonly property bool muted: sink && sink.audio ? sink.audio.muted : false
    readonly property int percent: sink && sink.audio ? Math.round(sink.audio.volume * 100) : 0

    function icon() {
        if (percent === 0)
            return ""
        if (percent < 50)
            return ""
        return ""
    }

    PwObjectTracker {
        objects: [root.sink]
    }

    Text {
        id: volumeText
        anchors.centerIn: parent
        text: !parent.sink ? "--% " : parent.muted ? "" : `${parent.percent}% ${parent.icon()}`
        color: "white"
        font.family: "Hack Nerd Font Propo"
        font.pixelSize: 14
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
