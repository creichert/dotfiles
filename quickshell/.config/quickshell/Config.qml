import QtQml
import Quickshell

QtObject {
    // Host-specific display and density settings
    readonly property bool isLaptop: Quickshell.env("HOSTNAME") === "laptop"

    property string primaryMonitor: isLaptop ? "eDP-1" : "DP-1"
    property int barHeight: isLaptop ? 25 : 30
    property int fontPixelSize: isLaptop ? 12 : 14
    property string fontFamily: "Hack Nerd Font Propo"
    property bool networkModuleEnabled: true
    property bool batteryModuleEnabled: isLaptop
    property bool brightnessModuleEnabled: isLaptop

    // Theme and UX contract
    property string surfaceBaseColor: "#272a2c"
    property string surfaceRaisedColor: "#323638"
    property string surfaceSelectedColor: "#424847"
    property string textPrimaryColor: "#e8dfc8"
    property string textMutedColor: "#b6ac93"
    property string accentActiveColor: "#b5e78f"
    property string accentActiveDeepColor: "#36a65c"
    property string borderColor: "#58665b"
    property string separatorColor: "#3d4741"
    property string urgentColor: "#d64a42"
    property int surfaceRadius: 4
    property int controlRadius: 3

    // Existing component defaults
    property string textColor: textPrimaryColor
    property string barBackgroundColor: surfaceBaseColor
    property string activeBackgroundColor: surfaceSelectedColor
    property string accentColor: accentActiveColor
    property string urgentBackgroundColor: urgentColor
    property string inhibitedBackgroundColor: surfaceSelectedColor
    property string inhibitedTextColor: textPrimaryColor

    // Bar/group gaps, within-widget gaps, status insets, and fixed control slots.
    //
    // This is the actual layout gap between neighboring things. Workspaces now
    // use it between their fixed slots, and the tray uses it between tray
    // items.
    property int barSpacing: 4
    // Gap from the fixed Clock anchor to either center satellite area.
    property int barCenterSpacing: 8
    // This is inside one widget. Audio uses it between transient % and speaker;
    // Network between its internal elements; Resources between value and glyph.
    property int barContentSpacing: 4
    // This is what Audio, Resources, Network and Notifications now use to give
    // their visible content breathing room from the module boundary.
    property int barStatusHorizontalInset: 8
    // The fixed geometry for things that are actual icon slots/buttons:
    // Workspaces and tray items currently use it.
    property int barIconButtonWidth: 32
    property int trayIconSize: 18

    // Quickshell is the session notification daemon.
    property bool notificationServerEnabled: true
    property int notificationToastTimeout: 5000
    property int notificationMaximumVisible: 3
    property int notificationHistoryLimit: 100
    property int notificationWidth: 400
    property int notificationCenterWidth: 480
    property int notificationCenterHeight: 720
    property int notificationSpacing: 8
    property int notificationMargin: 12
    property string notificationBackgroundColor: surfaceRaisedColor

    // Application launcher
    // Null launches parsed Exec commands directly. Those inherit Quickshell's
    // service cgroup and do not currently honor Terminal=true. A nonempty
    // prefix receives a .desktop[:action] reference and can provide that policy.
    property var launcherCommandPrefix: ["uwsm", "app", "-s", "a", "--"]
    property var launcherExcludedEntries: ["Emacs (Client)", "Avahi", "Hardware Locality","Qt"]
    property bool launcherDesktopActionsEnabled: true
    property real launcherWidthRatio: 0.5
    property real launcherHeightRatio: 0.4
    property int launcherMaximumWidth: 900
    property int launcherMaximumHeight: 560
    property int launcherTitleFontPixelSize: 18
    property int launcherSubtitleFontPixelSize: 14
    property int launcherRowHeight: 48
    property int launcherActionRowHeight: 44

    // Clipboard history picker
    property real clipboardWidthRatio: 0.65
    property real clipboardHeightRatio: 0.55
    property int clipboardMaximumWidth: 1100
    property int clipboardMaximumHeight: 700
    property real clipboardListWidthRatio: 0.42
    property int clipboardRowHeight: 58
    property int clipboardPreviewMaximumCharacters: 8000

    // Workspace presentation
    property var workspaceIcons: ({
        "1": "browser",
        "2": "terminal",
        "3": "code",
        "4": "agent",
        "5": "music",
        "cfg": "config",

        "terms": "terminalWindow",
        "db": "database",
        "default": "workspaceDefault",
        "urgent": "warning"
    })

    // Module behavior
    property string clockFormat: "ddd dd · HH:mm"
    property int titleMaximumWidth: isLaptop ? 500 : 900
    property int titleSpacing: 6
    property string networkRateWidthLabel: "999.9 Mb/s"
    property int volumeMediumThreshold: 35
    property int volumeHighThreshold: 75
    property int batteryCriticalThreshold: 15
    property int batteryPanelWidth: 340
    // Personal bar attention policy, not a Linux memory-pressure measurement.
    property int memoryAttentionThreshold: 90
    // Host-specific presentation policy; tune against ordinary workloads.
    property int temperatureCoolThreshold: isLaptop ? 35 : 50
    property int temperatureWarmThreshold: isLaptop ? 50 : 70
    property int temperatureCriticalThreshold: isLaptop ? 65 : 85

    // Host metrics
    property string cpuTemperatureHwmonPath: isLaptop
        ? "/sys/devices/platform/coretemp.0/hwmon"
        : "/sys/bus/pci/drivers/k10temp/0000:00:18.3/hwmon"
    property real metricsIntervalSeconds: 2
    property int temperatureIntervalSamples: 3
}
