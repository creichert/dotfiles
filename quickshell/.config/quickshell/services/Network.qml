// qmllint disable signal-handler-parameters

import QtQuick
import Quickshell.Io
import Quickshell.Networking

Item {
    id: root

    required property var metrics
    property bool panelVisible: false
    property string localIpv4: ""
    property bool detailsPending: false
    property bool queuedRefresh: false
    property var scanningDevice: null

    readonly property string interfaceName: metrics.interfaceName
    readonly property string gateway: metrics.gateway
    readonly property string linkState: metrics.linkState
    readonly property real receiveBytesPerSecond: metrics.receiveBytesPerSecond
    readonly property real transmitBytesPerSecond: metrics.transmitBytesPerSecond
    readonly property string statusText: !interfaceName ? "No default route"
        : linkState === "up" ? "Link up · default route"
        : linkState === "down" ? "Link down · default route"
        : linkState === "dormant" ? "Link dormant · default route"
        : "Default route present · link unknown"

    readonly property bool networkManagerAvailable: Networking.backend === NetworkBackendType.NetworkManager
    // Prefer a connected managed adapter, otherwise use the first managed Wi-Fi adapter.
    readonly property var wifiDevice: {
        if (!networkManagerAvailable)
            return null

        let first = null
        for (const device of Networking.devices.values) {
            if (device.type !== DeviceType.Wifi || !device.nmManaged)
                continue
            if (device.connected)
                return device
            if (!first)
                first = device
        }
        return first
    }
    readonly property bool wifiManagementAvailable: wifiDevice !== null
    readonly property var wifiEnabled: wifiManagementAvailable ? Networking.wifiEnabled : null
    readonly property var wifiHardwareBlocked: wifiManagementAvailable
        ? !Networking.wifiHardwareEnabled : null
    readonly property bool wifiConnected: wifiDevice ? wifiDevice.connected : false
    readonly property var connectedWifiNetwork: wifiConnected
        ? wifiDevice.networks.values.find(network => network.connected) || null : null
    readonly property string wifiSsid: connectedWifiNetwork ? connectedWifiNetwork.name : ""
    readonly property var wifiSignalPercent: connectedWifiNetwork
        && Number.isFinite(connectedWifiNetwork.signalStrength)
        ? Math.round(connectedWifiNetwork.signalStrength * 100) : null
    readonly property bool wifiCanScan: wifiManagementAvailable
        && wifiEnabled === true && wifiHardwareBlocked === false
    readonly property bool wifiScanning: scanningDevice === wifiDevice
        && wifiDevice !== null && wifiDevice.scannerEnabled
    readonly property var availableWifiNetworks: wifiScanning
        ? wifiDevice.networks.values.map(network => ({
            ssid: network.name || "SSID unavailable",
            signalPercent: Number.isFinite(network.signalStrength)
                ? Math.round(network.signalStrength * 100) : null,
            connected: network.connected,
            known: network.known,
            security: wifiSecurityLabel(network.security)
        })) : []

    function wifiSecurityLabel(security) {
        switch (security) {
        case WifiSecurityType.Open: return "Open"
        case WifiSecurityType.Owe: return "Enhanced open"
        case WifiSecurityType.Sae: return "WPA3"
        case WifiSecurityType.Wpa3SuiteB192: return "WPA3 Enterprise"
        case WifiSecurityType.Wpa2Psk: return "WPA2"
        case WifiSecurityType.Wpa2Eap: return "WPA2 Enterprise"
        case WifiSecurityType.WpaPsk: return "WPA"
        case WifiSecurityType.WpaEap: return "WPA Enterprise"
        case WifiSecurityType.StaticWep:
        case WifiSecurityType.DynamicWep: return "WEP"
        case WifiSecurityType.Leap: return "LEAP"
        default: return "Security unknown"
        }
    }

    function syncWifiScan() {
        const nextDevice = panelVisible && wifiCanScan ? wifiDevice : null
        if (scanningDevice && scanningDevice !== nextDevice)
            scanningDevice.scannerEnabled = false

        scanningDevice = nextDevice
        if (nextDevice && !nextDevice.scannerEnabled)
            nextDevice.scannerEnabled = true
    }

    function rate(bytes) {
        const bits = bytes * 8
        if (bits < 1000)
            return `${Math.round(bits)} b/s`
        if (bits < 1000000)
            return `${(bits / 1000).toFixed(1)} Kb/s`
        return `${(bits / 1000000).toFixed(1)} Mb/s`
    }

    function refreshAddress() {
        localIpv4 = ""
        if (!panelVisible || !interfaceName) {
            queuedRefresh = false
            detailsPending = false
            return
        }

        detailsPending = true
        if (addressProcess.running) {
            queuedRefresh = true
            return
        }

        addressProcess.requestedInterface = interfaceName
        addressProcess.exec(["ip", "-j", "-4", "address", "show", "dev", interfaceName])
    }

    function updateAddress(output) {
        if (!panelVisible || addressProcess.requestedInterface !== interfaceName)
            return

        try {
            const devices = JSON.parse(output)
            const addresses = devices.length > 0 ? devices[0].addr_info || [] : []
            const address = addresses.find(entry => entry.family === "inet" && entry.scope === "global")
            localIpv4 = address ? address.local : ""
        } catch (error) {
            console.warn("Ignoring invalid interface address data:", error)
            localIpv4 = ""
        }
    }

    function togglePanel() {
        if (panelVisible)
            closePanel()
        else {
            panelVisible = true
            refreshAddress()
        }
    }

    function closePanel() {
        panelVisible = false
        queuedRefresh = false
        detailsPending = false
        addressProcess.running = false
    }

    onInterfaceNameChanged: {
        localIpv4 = ""
        if (panelVisible)
            refreshAddress()
    }

    onLinkStateChanged: {
        if (panelVisible && linkState === "up" && !detailsPending)
            refreshAddress()
    }

    onPanelVisibleChanged: syncWifiScan()
    onWifiDeviceChanged: syncWifiScan()
    onWifiEnabledChanged: syncWifiScan()
    onWifiHardwareBlockedChanged: syncWifiScan()

    Process {
        id: addressProcess

        property string requestedInterface: ""

        stdout: StdioCollector {
            onStreamFinished: root.updateAddress(text)
        }

        onExited: exitCode => {
            if (root.queuedRefresh && root.panelVisible) {
                root.queuedRefresh = false
                Qt.callLater(() => root.refreshAddress())
            } else {
                if (exitCode !== 0 && requestedInterface === root.interfaceName)
                    root.localIpv4 = ""
                root.detailsPending = false
            }
        }
    }

    IpcHandler {
        target: "network"

        function togglePanel(): void {
            root.togglePanel()
        }
    }
}
