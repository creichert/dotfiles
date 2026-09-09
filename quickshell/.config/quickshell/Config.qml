import QtQml

QtObject {
    // Display and appearance
    property string primaryMonitor: "DP-1"
    property int barHeight: 30
    property int fontPixelSize: 14
    property string fontFamily: "Hack Nerd Font Propo"
    property string textColor: "white"
    property string barBackgroundColor: "#802b303b"
    property string activeBackgroundColor: "#64727d"
    property string accentColor: "#32ccffe6"
    property string urgentBackgroundColor: "#eb4d4b"
    property string inhibitedBackgroundColor: "#ecf0f1"
    property string inhibitedTextColor: "#2d3436"
    property int barSpacing: 4
    property int moduleHorizontalPadding: 16
    property int trayIconSize: 18

    // Quickshell is the session notification daemon.
    property bool notificationServerEnabled: true
    property int notificationToastTimeout: 5000
    property int notificationMaximumVisible: 3
    property int notificationMaximumQueued: 20
    property int notificationHistoryLimit: 100
    property int notificationWidth: 400
    property int notificationCenterHeight: 600
    property int notificationSpacing: 8
    property int notificationMargin: 12
    property string notificationBackgroundColor: "#e92b303b"

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

    // Workspace presentation
    property int workspaceHorizontalPadding: 10
    property var workspaceIcons: ({
        "1": "",
        "2": "",
        "3": "",
        "4": "",
        "cfg": "",
        "terms": "",
        "db": "",
        "default": "",
        "urgent": ""
    })

    // Module behavior
    property string clockFormat: "MM/dd/yyyy HH:mm"
    property int titleMaximumWidth: 900
    property int titleSpacing: 6
    property int networkSpacing: 6
    property string networkRateWidthLabel: "999.9 Mb/s"
    property int volumeMediumThreshold: 50
    property int temperatureCoolThreshold: 50
    property int temperatureWarmThreshold: 70
    property int temperatureCriticalThreshold: 85

    // Host metrics
    property string cpuTemperatureHwmonPath: "/sys/bus/pci/drivers/k10temp/0000:00:18.3/hwmon"
    property real metricsIntervalSeconds: 2
    property int temperatureIntervalSamples: 3
}
