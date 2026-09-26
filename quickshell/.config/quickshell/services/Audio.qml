import QtQuick
import Quickshell.Services.Pipewire

Item {
    id: root

    readonly property var sink: Pipewire.ready ? Pipewire.defaultAudioSink : null
    readonly property var source: Pipewire.ready ? Pipewire.defaultAudioSource : null
    readonly property var outputDevices: Pipewire.nodes.values.filter(node =>
        node.audio && node.isSink && !node.isStream)
    readonly property bool sinkMuted: sink && sink.audio ? sink.audio.muted : false
    readonly property real sinkVolume: sink && sink.audio ? sink.audio.volume : 0
    readonly property bool sourceMuted: source && source.audio ? source.audio.muted : false
    readonly property real sourceVolume: source && source.audio ? source.audio.volume : 0

    PwObjectTracker {
        objects: [root.sink, root.source]
    }

    function setSinkVolume(volume) {
        if (sink && sink.ready && sink.audio && Number.isFinite(volume))
            sink.audio.volume = Math.max(0, Math.min(1, volume))
    }

    function adjustSinkVolume(delta) {
        if (sink && sink.ready && sink.audio)
            setSinkVolume(sink.audio.volume + delta)
    }

    function setSinkMuted(muted) {
        if (sink && sink.ready && sink.audio)
            sink.audio.muted = muted
    }

    function toggleSinkMuted() {
        if (sink && sink.ready && sink.audio)
            setSinkMuted(!sink.audio.muted)
    }

    function selectOutputDevice(device) {
        if (device && outputDevices.indexOf(device) !== -1)
            Pipewire.preferredDefaultAudioSink = device
    }

    function setSourceVolume(volume) {
        if (source && source.ready && source.audio && Number.isFinite(volume))
            source.audio.volume = Math.max(0, Math.min(1, volume))
    }

    function setSourceMuted(muted) {
        if (source && source.ready && source.audio)
            source.audio.muted = muted
    }
}
