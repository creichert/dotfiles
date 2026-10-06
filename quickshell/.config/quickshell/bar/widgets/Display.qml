// Display feature entry point; brightness is currently its only capability.
import QtQuick
import "../../components" as Components

Components.BarItem {
    id: root

    required property var display
    visible: config.brightnessModuleEnabled && display.brightnessAvailable
    engaged: display.panelVisible

    contentItem: Components.Icon {
        name: root.display.brightnessIconName
        theme: root.theme
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
