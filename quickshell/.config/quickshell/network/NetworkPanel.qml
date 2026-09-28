pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls as Controls
import QtQuick.Layouts
import "../components" as Components

Components.AnchoredPanel {
    id: root

    required property var network
    required property var theme
    property string view: "status"

    implicitWidth: 380
    implicitHeight: network.wifiManagementAvailable ? 580 : 365
    gap: theme.spacingMedium
    visible: network.panelVisible
    onDismissed: network.closePanel()

    onVisibleChanged: {
        if (!visible) {
            view = "status"
            if (network.panelVisible)
                network.closePanel()
            return
        }

        focusPanel()
    }

    onViewChanged: focusPanel()

    function focusPanel() {
        Qt.callLater(() => {
            if (root.visible)
                panelFocus.forceActiveFocus()
        })
    }

    function showStatus() {
        view = "status"
    }

    function showDiscovery() {
        if (network.wifiManagementAvailable)
            view = "discovery"
    }

    Binding {
        target: root.network
        property: "discoveryRequested"
        value: root.visible && root.view === "discovery"
    }

    Connections {
        target: root.network

        function onWifiManagementAvailableChanged() {
            if (!root.network.wifiManagementAvailable && root.view === "discovery")
                root.showStatus()
        }
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
                if (root.view === "discovery")
                    root.showStatus()
                else
                    root.network.closePanel()
                event.accepted = true
            }

            RowLayout {
                id: header

                anchors.top: parent.top
                width: parent.width
                spacing: root.theme.spacingMedium

                Components.Button {
                    visible: root.view === "discovery"
                    theme: root.theme
                    text: "Back"
                    implicitHeight: 32
                    onClicked: root.showStatus()
                }

                Text {
                    Layout.fillWidth: true
                    text: root.view === "discovery" ? "Available networks" : "Network"
                    color: root.theme.primaryText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.titleFontPixelSize + 2
                    font.bold: true
                }

                Controls.BusyIndicator {
                    visible: running
                    running: root.view === "discovery" && root.network.wifiScanning
                    implicitWidth: 20
                    implicitHeight: 20
                }
            }

            Loader {
                anchors {
                    top: header.bottom
                    topMargin: root.theme.sectionSpacing
                    left: parent.left
                    right: parent.right
                    bottom: parent.bottom
                }
                active: root.visible
                sourceComponent: root.view === "discovery" ? discoveryView : statusView
            }

            Component {
                id: statusView

                Flickable {
                    id: contents

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

                        Rectangle {
                            width: parent.width
                            height: 1
                            color: root.theme.separator
                            visible: root.network.wifiManagementAvailable
                        }

                        Column {
                            width: parent.width
                            spacing: root.theme.spacingMedium + 2
                            visible: root.network.wifiManagementAvailable

                            RowLayout {
                                width: parent.width

                                Text {
                                    Layout.fillWidth: true
                                    text: "Wi-Fi"
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize + 2
                                    font.bold: true
                                }

                                Text {
                                    text: root.network.wifiHardwareBlocked ? "Hardware blocked"
                                        : root.network.wifiEnabled ? "On" : "Off"
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                }
                            }

                            RowLayout {
                                width: parent.width

                                Text {
                                    Layout.fillWidth: true
                                    text: "Network"
                                    color: root.theme.mutedText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                }

                                Text {
                                    Layout.maximumWidth: parent.width * 0.7
                                    text: root.network.wifiSsid || (root.network.wifiConnected
                                        ? "Connected · SSID unavailable" : "Not connected")
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                    elide: Text.ElideRight
                                }
                            }

                            RowLayout {
                                width: parent.width
                                visible: root.network.wifiSignalPercent !== null

                                Text {
                                    Layout.fillWidth: true
                                    text: "Signal"
                                    color: root.theme.mutedText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                }

                                Text {
                                    text: root.network.wifiSignalPercent !== null
                                        ? `${root.network.wifiSignalPercent}%` : ""
                                    color: root.theme.primaryText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                }
                            }

                            Components.Button {
                                width: parent.width
                                implicitHeight: 42
                                theme: root.theme
                                text: "Available networks"
                                onClicked: root.showDiscovery()

                                contentItem: RowLayout {
                                    spacing: root.theme.spacingMedium

                                    Text {
                                        Layout.fillWidth: true
                                        text: "Available networks"
                                        color: root.theme.primaryText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.bodyFontPixelSize
                                    }

                                    Text {
                                        text: "›"
                                        color: root.theme.mutedText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.bodyFontPixelSize + 4
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Component {
                id: discoveryView

                Item {
                    Text {
                        anchors.centerIn: parent
                        visible: !root.network.wifiCanScan || networksList.count === 0
                        text: root.network.wifiHardwareBlocked ? "Wi-Fi hardware blocked"
                            : !root.network.wifiEnabled ? "Wi-Fi is off"
                            : root.network.wifiScanning ? "No networks found yet"
                            : "Scanning unavailable"
                        color: root.theme.mutedText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.bodyFontPixelSize
                    }

                    ListView {
                        id: networksList

                        anchors.fill: parent
                        visible: root.network.wifiCanScan && count > 0
                        clip: true
                        spacing: 2
                        activeFocusOnTab: true
                        keyNavigationEnabled: true
                        model: root.network.availableWifiNetworks

                        delegate: Rectangle {
                            id: networkEntry

                            required property var modelData

                            width: networksList.width
                            height: 48
                            radius: root.theme.controlRadius
                            color: networkEntry.modelData.connected
                                ? root.theme.selectedSurface : root.theme.surface

                            RowLayout {
                                anchors {
                                    fill: parent
                                    leftMargin: root.theme.spacingMedium
                                    rightMargin: root.theme.spacingMedium
                                }
                                spacing: root.theme.spacingMedium

                                Column {
                                    Layout.fillWidth: true
                                    spacing: root.theme.spacingSmall

                                    Text {
                                        width: parent.width
                                        text: networkEntry.modelData.ssid
                                        color: root.theme.primaryText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.bodyFontPixelSize
                                        font.bold: networkEntry.modelData.connected
                                        elide: Text.ElideRight
                                    }

                                    Text {
                                        width: parent.width
                                        text: (networkEntry.modelData.connected ? "Connected · " : "")
                                            + (networkEntry.modelData.known ? "Saved · " : "Not saved · ")
                                            + networkEntry.modelData.security
                                        color: root.theme.mutedText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.secondaryFontPixelSize
                                        elide: Text.ElideRight
                                    }
                                }

                                Text {
                                    text: networkEntry.modelData.signalPercent !== null
                                        ? `${networkEntry.modelData.signalPercent}%` : "—"
                                    color: root.theme.mutedText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.bodyFontPixelSize
                                }
                            }
                        }
                    }

                    Rectangle {
                        anchors.fill: networksList
                        visible: networksList.visible && networksList.activeFocus
                        enabled: false
                        color: "transparent"
                        border.width: 1
                        border.color: root.theme.activeAccent
                        radius: root.theme.controlRadius
                    }
                }
            }
        }
    }
}
