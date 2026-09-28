pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var network
    required property var theme

    implicitWidth: 380
    implicitHeight: 365
    gap: theme.spacingMedium
    visible: network.panelVisible
    onDismissed: network.closePanel()

    onVisibleChanged: {
        if (!visible) {
            if (network.panelVisible)
                network.closePanel()
            return
        }

        Qt.callLater(() => {
            if (root.visible)
                panelFocus.forceActiveFocus()
        })
    }

    Rectangle {
        anchors.fill: parent
        radius: root.theme.surfaceRadius
        color: root.theme.raisedSurface
        border.width: 1
        border.color: root.theme.separator

        MouseArea {
            anchors.fill: parent
        }

        FocusScope {
            id: panelFocus

            anchors {
                fill: parent
                margins: root.theme.panelPadding
            }
            focus: true

            Keys.onEscapePressed: event => {
                root.network.closePanel()
                event.accepted = true
            }

            Text {
                id: title

                anchors.top: parent.top
                text: "Network"
                color: root.theme.primaryText
                font.family: root.theme.fontFamily
                font.pixelSize: root.theme.titleFontPixelSize + 2
                font.bold: true
            }

            Flickable {
                id: contents

                anchors {
                    top: title.bottom
                    topMargin: root.theme.sectionSpacing
                    left: parent.left
                    right: parent.right
                    bottom: parent.bottom
                }
                contentWidth: width
                contentHeight: sections.implicitHeight
                clip: true
                interactive: contentHeight > height

                Column {
                    id: sections

                    width: contents.width
                    spacing: root.theme.sectionSpacing

                    RowLayout {
                        width: parent.width
                        spacing: root.theme.spacingLarge

                        Components.Icon {
                            name: root.network.interfaceName && root.network.linkState !== "down"
                                ? "networkConnected" : "networkDisconnected"
                            theme: root.theme
                            font.pixelSize: root.theme.bodyFontPixelSize + 8
                        }

                        Column {
                            Layout.fillWidth: true
                            spacing: root.theme.spacingSmall

                            Text {
                                width: parent.width
                                text: root.network.interfaceName || "No active interface"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize + 2
                                font.bold: true
                                elide: Text.ElideRight
                            }

                            Text {
                                width: parent.width
                                text: root.network.statusText
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                                elide: Text.ElideRight
                            }
                        }
                    }

                    Rectangle {
                        width: parent.width
                        height: 1
                        color: root.theme.separator
                    }

                    Column {
                        width: parent.width
                        spacing: root.theme.spacingMedium + 2

                        Text {
                            text: "Connection"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize + 2
                            font.bold: true
                        }

                        RowLayout {
                            width: parent.width
                            visible: root.network.interfaceName.length > 0

                            Text {
                                Layout.fillWidth: true
                                text: "Local IPv4"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Text {
                                text: root.network.detailsPending ? "Loading…"
                                    : root.network.localIpv4 || "Unavailable"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }
                        }

                        RowLayout {
                            width: parent.width
                            visible: root.network.gateway.length > 0

                            Text {
                                Layout.fillWidth: true
                                text: "Gateway"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Text {
                                text: root.network.gateway
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }
                        }

                        Text {
                            text: "Internet reachability not checked"
                            color: root.theme.mutedText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.secondaryFontPixelSize
                        }
                    }

                    Rectangle {
                        width: parent.width
                        height: 1
                        color: root.theme.separator
                    }

                    Column {
                        width: parent.width
                        spacing: root.theme.spacingMedium + 2

                        Text {
                            text: "Traffic"
                            color: root.theme.primaryText
                            font.family: root.theme.fontFamily
                            font.pixelSize: root.theme.bodyFontPixelSize + 2
                            font.bold: true
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Components.Icon {
                                name: "download"
                                theme: root.theme
                                font.pixelSize: root.theme.bodyFontPixelSize + 4
                            }

                            Text {
                                Layout.fillWidth: true
                                text: "Download"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Text {
                                text: root.network.interfaceName
                                    ? root.network.rate(root.network.receiveBytesPerSecond) : "—"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }
                        }

                        RowLayout {
                            width: parent.width
                            spacing: root.theme.spacingMedium

                            Components.Icon {
                                name: "upload"
                                theme: root.theme
                                font.pixelSize: root.theme.bodyFontPixelSize + 4
                            }

                            Text {
                                Layout.fillWidth: true
                                text: "Upload"
                                color: root.theme.mutedText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }

                            Text {
                                text: root.network.interfaceName
                                    ? root.network.rate(root.network.transmitBytesPerSecond) : "—"
                                color: root.theme.primaryText
                                font.family: root.theme.fontFamily
                                font.pixelSize: root.theme.bodyFontPixelSize
                            }
                        }
                    }
                }
            }
        }
    }
}
