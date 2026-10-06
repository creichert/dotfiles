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

    implicitWidth: Math.min(380, availableWidth)
    implicitHeight: Math.min(!network.wifiManagementAvailable ? 365
        : view === "discovery" ? 580 : 520, availableHeight)
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

        function onWifiConnectionSucceeded() {
            if (root.view === "discovery")
                root.showStatus()
        }
    }

    Rectangle {
        anchors.fill: parent
        radius: root.theme.surfaceRadius
        color: root.theme.surface
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

                Components.IconButton {
                    visible: root.view === "discovery"
                    Layout.alignment: Qt.AlignVCenter
                    theme: root.theme
                    text: "Back"
                    iconName: "chevronLeft"
                    iconPixelSize: root.theme.bodyFontPixelSize + 2
                    implicitWidth: 32
                    implicitHeight: 32
                    onClicked: root.showStatus()
                }

                Text {
                    Layout.fillWidth: true
                    Layout.alignment: Qt.AlignVCenter
                    text: root.view === "discovery" ? "Available networks" : "Network"
                    color: root.theme.primaryText
                    font.family: root.theme.fontFamily
                    font.pixelSize: root.theme.titleFontPixelSize + 2
                    font.bold: true
                }

                Controls.BusyIndicator {
                    Layout.alignment: Qt.AlignVCenter
                    visible: running
                    running: root.view === "discovery" && root.network.wifiCanScan
                        && root.network.wifiScanning
                    implicitWidth: 28
                    implicitHeight: 28
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
                                    visible: root.network.wifiHardwareBlocked
                                    text: "Hardware blocked"
                                    color: root.theme.mutedText
                                    font.family: root.theme.fontFamily
                                    font.pixelSize: root.theme.secondaryFontPixelSize
                                }

                                Components.Switch {
                                    theme: root.theme
                                    text: "Wi-Fi"
                                    checked: root.network.wifiEnabled === true
                                    enabled: !root.network.wifiHardwareBlocked
                                    onToggled: root.network.setWifiEnabled(checked)
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
                                id: availableNetworksButton

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

                                    Components.Icon {
                                        theme: root.theme
                                        name: "chevronRight"
                                        color: root.theme.mutedText
                                        font.pixelSize: root.theme.bodyFontPixelSize + 2
                                    }
                                }

                                background: Rectangle {
                                    radius: root.theme.controlRadius
                                    color: availableNetworksButton.pressed ? root.theme.surface
                                        : availableNetworksButton.hovered
                                            ? root.theme.selectedSurface : "transparent"
                                    border.width: availableNetworksButton.visualFocus ? 1 : 0
                                    border.color: root.theme.activeAccent
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
                        id: connectionMessage

                        anchors {
                            top: parent.top
                            left: parent.left
                            right: parent.right
                        }
                        visible: root.network.pendingWifiNetwork !== null
                            || root.network.wifiConnectionError.length > 0
                        height: visible ? implicitHeight : 0
                        text: root.network.pendingWifiNetwork
                            ? `Connecting to ${root.network.pendingWifiNetwork.name}…`
                            : root.network.wifiConnectionError
                        color: root.theme.mutedText
                        font.family: root.theme.fontFamily
                        font.pixelSize: root.theme.secondaryFontPixelSize
                        elide: Text.ElideRight
                    }

                    Text {
                        anchors.centerIn: networksList
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

                        anchors {
                            top: connectionMessage.bottom
                            topMargin: connectionMessage.visible ? root.theme.spacingSmall : 0
                            left: parent.left
                            right: parent.right
                            bottom: parent.bottom
                        }
                        visible: root.network.wifiCanScan && count > 0
                        clip: true
                        spacing: 2
                        activeFocusOnTab: false
                        keyNavigationEnabled: false
                        model: root.network.availableWifiNetworkModel
                        currentIndex: -1
                        property var keyboardTarget: null

                        function focusNetwork(network) {
                            const index = model ? model.values.indexOf(network) : -1
                            if (index < 0)
                                return
                            keyboardTarget = network
                            currentIndex = index
                            positionViewAtIndex(index, ListView.Contain)
                            focusKeyboardTarget()
                        }

                        function focusKeyboardTarget() {
                            if (keyboardTarget && currentItem && model
                                    && model.values[currentIndex] === keyboardTarget
                                    && currentItem.enabled) {
                                currentItem.forceActiveFocus()
                                keyboardTarget = null
                            }
                        }

                        function focusAdjacent(index, direction) {
                            const networks = model ? model.values : []
                            for (let next = index + direction; next >= 0 && next < networks.length;
                                    next += direction) {
                                if (root.network.canConnectWifiNetwork(networks[next])) {
                                    focusNetwork(networks[next])
                                    return
                                }
                            }
                        }

                        onCurrentItemChanged: focusKeyboardTarget()

                        delegate: Components.Button {
                            id: networkEntry

                            required property int index
                            required property var modelData
                            readonly property var details: root.network.wifiNetworkDetails(modelData)

                            theme: root.theme
                            width: networksList.width
                            height: 48
                            text: networkEntry.details.ssid
                            enabled: root.network.pendingWifiNetwork === null
                                && root.network.canConnectWifiNetwork(modelData)
                            activeFocusOnTab: enabled
                            leftPadding: root.theme.spacingMedium
                            rightPadding: root.theme.spacingMedium
                            topPadding: 0
                            bottomPadding: 0
                            onClicked: root.network.connectWifiNetwork(modelData)

                            onActiveFocusChanged: {
                                if (activeFocus && networksList.currentIndex !== index)
                                    networksList.currentIndex = index
                            }

                            Keys.onUpPressed: event => {
                                networksList.focusAdjacent(networkEntry.index, -1)
                                event.accepted = true
                            }
                            Keys.onDownPressed: event => {
                                networksList.focusAdjacent(networkEntry.index, 1)
                                event.accepted = true
                            }
                            Keys.onReturnPressed: event => {
                                networkEntry.click()
                                event.accepted = true
                            }
                            Keys.onEnterPressed: event => {
                                networkEntry.click()
                                event.accepted = true
                            }

                            background: Rectangle {
                                radius: root.theme.controlRadius
                                color: networkEntry.details.connected
                                    || (networkEntry.hovered && networkEntry.enabled)
                                    ? root.theme.selectedSurface : root.theme.surface
                                border.width: networkEntry.visualFocus ? 1 : 0
                                border.color: root.theme.activeAccent
                            }

                            contentItem: RowLayout {
                                spacing: root.theme.spacingMedium

                                Column {
                                    Layout.fillWidth: true
                                    spacing: root.theme.spacingSmall

                                    Text {
                                        width: parent.width
                                        text: networkEntry.details.ssid
                                        color: root.theme.primaryText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.bodyFontPixelSize
                                        font.bold: networkEntry.details.connected
                                        elide: Text.ElideRight
                                    }

                                    Text {
                                        width: parent.width
                                        text: (networkEntry.details.connected ? "Connected · " : "")
                                            + (networkEntry.details.known ? "Saved · " : "Not saved · ")
                                            + networkEntry.details.security
                                            + (networkEntry.details.unavailable ? " · Unavailable here" : "")
                                        color: root.theme.mutedText
                                        font.family: root.theme.fontFamily
                                        font.pixelSize: root.theme.secondaryFontPixelSize
                                        elide: Text.ElideRight
                                    }
                                }

                                Text {
                                    text: networkEntry.details.signalPercent !== null
                                        ? `${networkEntry.details.signalPercent}%` : "—"
                                    color: root.theme.mutedText
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
}
