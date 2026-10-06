import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    required property var display
    visible: config.brightnessModuleEnabled && display.brightnessAvailable
    engaged: display.panelVisible

    Timer {
        id: feedbackTimer
        interval: 1800
        repeat: false
    }

    Connections {
        target: root.display

        function onBrightnessAdjustmentSucceeded() {
            feedbackTimer.restart()
        }
    }

    contentItem: Row {
        spacing: root.config.barContentSpacing

        Text {
            visible: feedbackTimer.running
            text: root.display.brightnessPercent !== null
                ? `${Math.round(root.display.brightnessPercent)}%` : "--%"
            color: root.theme.primaryText
            font.family: root.theme.fontFamily
            font.pixelSize: root.theme.fontPixelSize
        }

        Components.Icon {
            name: root.display.brightnessIconName
            theme: root.theme
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton
        onClicked: root.display.togglePanel()
        onWheel: wheel => {
            if (wheel.angleDelta.y !== 0)
                root.display.adjustBrightnessPercent(wheel.angleDelta.y > 0
                    ? root.config.brightnessStepPercent : -root.config.brightnessStepPercent)
        }
    }
}
