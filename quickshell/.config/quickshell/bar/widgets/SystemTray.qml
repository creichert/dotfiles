pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Services.SystemTray
import Quickshell.Widgets

Item {
    id: root

    required property var config
    implicitWidth: trayRow.implicitWidth
    implicitHeight: config.barHeight

    Row {
        id: trayRow

        anchors.centerIn: parent

        Repeater {
            model: SystemTray.items

            delegate: Item {
                id: trayItem

                required property var modelData
                implicitWidth: root.config.barHeight
                implicitHeight: root.config.barHeight

                IconImage {
                    anchors.centerIn: parent
                    source: trayItem.modelData.icon
                    implicitSize: root.config.trayIconSize
                }

                QsMenuAnchor {
                    id: menuAnchor

                    menu: trayItem.modelData.menu
                    anchor.item: trayItem
                }

                MouseArea {
                    anchors.fill: parent
                    acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton

                    onClicked: mouse => {
                        if (mouse.button === Qt.LeftButton) {
                            if (trayItem.modelData.onlyMenu && trayItem.modelData.hasMenu)
                                menuAnchor.open()
                            else
                                trayItem.modelData.activate()
                        } else if (mouse.button === Qt.MiddleButton) {
                            trayItem.modelData.secondaryActivate()
                        } else if (trayItem.modelData.hasMenu) {
                            menuAnchor.open()
                        }
                    }
                    onWheel: wheel => trayItem.modelData.scroll(wheel.angleDelta.y, false)
                }
            }
        }
    }
}
