import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    required property var audio
    engaged: audio.panelVisible

    readonly property var sink: audio.sink
    readonly property bool muted: audio.sinkMuted
    readonly property int percent: Math.round(audio.sinkVolume * 100)

    function icon() {
        if (percent === 0)
            return "volumeOff"
        if (percent < config.volumeMediumThreshold)
            return "volumeLow"
        if (percent < config.volumeHighThreshold)
            return "volumeMedium"
        return "volumeHigh"
    }

    Timer {
        id: feedbackTimer
        interval: 1800
        repeat: false
    }

    contentItem: Row {
        id: volumeRow
        spacing: root.config.barContentSpacing

        Text {
            visible: feedbackTimer.running
            text: !root.sink ? "--%" : `${root.percent}%`
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
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: mouse => {
            if (mouse.button === Qt.LeftButton)
                root.audio.togglePanel()
            else if (mouse.button === Qt.RightButton)
                root.audio.toggleSinkMuted()
        }
        onWheel: wheel => {
            if (wheel.angleDelta.y === 0 || !root.sink || !root.sink.ready || !root.sink.audio)
                return
            root.audio.adjustSinkVolume(wheel.angleDelta.y > 0 ? 0.03 : -0.03)
            feedbackTimer.restart()
        }
    }
}
