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
    property bool discoveryRequested: false
    property var scanningDevice: null
    property var pendingWifiNetwork: null
    property bool pendingWifiWasConnecting: false
    property string wifiConnectionError: ""

    signal wifiConnectionSucceeded()

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
    readonly property bool wifiScanning: panelVisible && discoveryRequested && wifiCanScan
        && scanningDevice === wifiDevice && wifiDevice !== null && wifiDevice.scannerEnabled
    // Keep the native model stable as scan results arrive. The view loads it only in Discovery.
    readonly property var availableWifiNetworkModel: panelVisible && discoveryRequested && wifiCanScan
        ? wifiDevice.networks : null

    function wifiNetworkDetails(network) {
        return {
            ssid: network.name || "SSID unavailable",
            signalPercent: Number.isFinite(network.signalStrength)
                ? Math.round(network.signalStrength * 100) : null,
            connected: network.connected,
            known: network.known,
            unavailable: !network.connected && !network.known
                && network.security !== WifiSecurityType.Open,
            security: wifiSecurityLabel(network.security)
        }
    }

    function setWifiEnabled(enabled) {
        if (wifiManagementAvailable && !wifiHardwareBlocked)
            Networking.wifiEnabled = enabled
    }

    function canConnectWifiNetwork(network) {
        return wifiCanScan && network && network.device === wifiDevice
            && !network.connected && !network.stateChanging
            && (network.known || network.security === WifiSecurityType.Open)
    }

    function connectWifiNetwork(network) {
        if (!panelVisible || !discoveryRequested || pendingWifiNetwork
                || !canConnectWifiNetwork(network))
            return

        wifiConnectionError = ""
        pendingWifiWasConnecting = network.state === ConnectionState.Connecting
        pendingWifiNetwork = network
        network.connect()
    }

    function clearWifiAttempt() {
        pendingWifiNetwork = null
        pendingWifiWasConnecting = false
        wifiConnectionError = ""
    }

    function failWifiAttempt(message) {
        if (!pendingWifiNetwork)
            return
        pendingWifiNetwork = null
        pendingWifiWasConnecting = false
        wifiConnectionError = message
    }

    function wifiFailureMessage(reason) {
        switch (reason) {
        case ConnectionFailReason.NoSecrets: return "Saved credentials unavailable"
        case ConnectionFailReason.WifiAuthTimeout: return "Wi-Fi authentication timed out"
        case ConnectionFailReason.WifiNetworkLost: return "Network no longer available"
        default: return "Could not connect to Wi-Fi"
        }
    }

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
        const nextDevice = panelVisible && discoveryRequested && wifiCanScan ? wifiDevice : null
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

    onPanelVisibleChanged: {
        syncWifiScan()
        if (!panelVisible)
            clearWifiAttempt()
    }
    onDiscoveryRequestedChanged: {
        syncWifiScan()
        if (!discoveryRequested)
            clearWifiAttempt()
    }
    onWifiDeviceChanged: {
        syncWifiScan()
        if (pendingWifiNetwork && pendingWifiNetwork.device !== wifiDevice
                && !pendingWifiNetwork.connected)
            failWifiAttempt("Wi-Fi device changed")
    }
    onWifiEnabledChanged: {
        syncWifiScan()
        if (pendingWifiNetwork && !wifiEnabled)
            failWifiAttempt("Wi-Fi is off")
    }
    onWifiHardwareBlockedChanged: {
        syncWifiScan()
        if (pendingWifiNetwork && wifiHardwareBlocked)
            failWifiAttempt("Wi-Fi hardware blocked")
    }

    Connections {
        target: root.pendingWifiNetwork

        function onConnectedChanged() {
            if (root.pendingWifiNetwork && root.pendingWifiNetwork.connected) {
                root.clearWifiAttempt()
                root.wifiConnectionSucceeded()
            }
        }

        function onStateChanged() {
            const network = root.pendingWifiNetwork
            if (!network)
                return
            if (network.state === ConnectionState.Connecting)
                root.pendingWifiWasConnecting = true
            else if (network.state === ConnectionState.Disconnected && root.pendingWifiWasConnecting) {
                // Allow a more specific connectionFailed reason from the same NM update first.
                Qt.callLater(() => {
                    if (root.pendingWifiNetwork === network
                            && network.state === ConnectionState.Disconnected)
                        root.failWifiAttempt("Could not connect to Wi-Fi")
                })
            }
        }

        function onConnectionFailed(reason) {
            root.failWifiAttempt(root.wifiFailureMessage(reason))
        }

        function onDestroyed() {
            root.failWifiAttempt("Network no longer available")
        }
    }

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
