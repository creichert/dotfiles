pragma ComponentBehavior: Bound

import Quickshell
import "bar"
import "services"

ShellRoot {
    Config {
        id: config
    }

    Metrics {
        id: metricsService
        config: config
    }

    Variants {
        model: Quickshell.screens

        Bar {
            required property var modelData
            screen: modelData
            visible: modelData.name === config.primaryMonitor
            config: config
            metrics: metricsService
        }
    }
}
