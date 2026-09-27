import QtQuick
import Quickshell.Io
import Quickshell.Services.Pipewire

Item {
    id: root

    property bool panelVisible: false
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

    // Pipewire.preferredDefaultAudioSink = device
    //   ≈ user explicitly chooses preferred/default sink
    //   → WirePlumber remembers that choice
    //   → visible under `wpctl status` → Default Configured Devices
    //
    // wpctl clear-default
    //   → removes explicit default-device preference
    //   → WirePlumber resumes automatic device selection
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

    function togglePanel() {
        panelVisible = !panelVisible
    }

    function closePanel() {
        panelVisible = false
    }

    IpcHandler {
        target: "audio"

        function togglePanel(): void {
            root.togglePanel()
        }
    }
}
