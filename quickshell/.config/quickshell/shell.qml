pragma ComponentBehavior: Bound

import Quickshell
import "services"

ShellRoot {
    Metrics {
        id: metricsService
    }

    Variants {
        model: Quickshell.screens

        Bar {
            required property var modelData
            screen: modelData
            visible: modelData.name === "DP-1"
            metrics: metricsService
        }
    }
}
