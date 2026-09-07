pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications
import "../notifications" as Notifications

Item {
    id: root

    required property var config
    property alias visibleNotifications: state.visibleNotifications
    property alias queuedNotifications: state.queuedNotifications
    property alias lastDismissed: state.lastDismissed
    property alias history: state.history

    PersistentProperties {
        id: state

        reloadableId: "notificationController"
        property var visibleNotifications: []
        property var queuedNotifications: []
        property var lastDismissed: null
        property var history: []
    }

    function indexOfNotification(notifications, notification) {
        for (let index = 0; index < notifications.length; index++) {
            if (notifications[index].id === notification.id)
                return index
        }

        return -1
    }

    function regularVisibleCount() {
        let count = 0

        for (const notification of visibleNotifications) {
            if (notification.urgency !== NotificationUrgency.Critical)
                count++
        }

        return count
    }

    function removeFrom(notifications, notification) {
        const index = indexOfNotification(notifications, notification)

        if (index === -1)
            return notifications

        const next = notifications.slice()
        next.splice(index, 1)
        return next
    }

    function removeNotification(notification) {
        visibleNotifications = removeFrom(visibleNotifications, notification)
        queuedNotifications = removeFrom(queuedNotifications, notification)

        if (lastDismissed && lastDismissed.id === notification.id)
            lastDismissed = null

        showQueuedNotifications()
    }

    function recordNotification(notification) {
        const record = {
            id: notification.id,
            appName: notification.appName,
            appIcon: notification.appIcon,
            summary: notification.summary,
            body: notification.body,
            urgency: notification.urgency,
            timestamp: Date.now()
        }
        const index = history.findIndex(existing => existing.id === notification.id)
        const next = history.slice()

        if (index === -1)
            next.push(record)
        else
            next[index] = record

        if (next.length > config.notificationHistoryLimit)
            next.splice(0, next.length - config.notificationHistoryLimit)

        history = next
    }

    function showNotification(notification) {
        if (indexOfNotification(visibleNotifications, notification) !== -1)
            return

        if (notification.urgency === NotificationUrgency.Critical
                || regularVisibleCount() < config.notificationMaximumVisible) {
            visibleNotifications = visibleNotifications.concat(notification)
        } else if (indexOfNotification(queuedNotifications, notification) === -1
                && queuedNotifications.length < config.notificationMaximumQueued) {
            queuedNotifications = queuedNotifications.concat(notification)
        }
    }

    function showQueuedNotifications() {
        while (queuedNotifications.length > 0
                && regularVisibleCount() < config.notificationMaximumVisible) {
            const next = queuedNotifications[0]
            queuedNotifications = queuedNotifications.slice(1)
            visibleNotifications = visibleNotifications.concat(next)
        }
    }

    function trackNotification(notification) {
        notification.tracked = true
        recordNotification(notification)

        // Replacement notifications reuse an ID and keep their current slot.
        const wasVisible = indexOfNotification(visibleNotifications, notification) !== -1
        visibleNotifications = removeFrom(visibleNotifications, notification)
        queuedNotifications = removeFrom(queuedNotifications, notification)

        if (wasVisible)
            visibleNotifications = visibleNotifications.concat(notification)
        else
            showNotification(notification)
    }

    function hideVisibleNotifications() {
        if (visibleNotifications.length === 0)
            return

        lastDismissed = visibleNotifications[visibleNotifications.length - 1]
        visibleNotifications = []
        showQueuedNotifications()
    }

    function restoreLastDismissed() {
        if (!lastDismissed)
            return

        const notification = lastDismissed
        lastDismissed = null
        showNotification(notification)
    }

    function dismissNotification(notification) {
        notification.dismiss()
    }

    IpcHandler {
        target: "notifications"

        function dismissVisible(): void {
            root.hideVisibleNotifications()
        }

        function restoreLastDismissed(): void {
            root.restoreLastDismissed()
        }
    }

    NotificationServer {
        id: server

        keepOnReload: true
        actionsSupported: true
        bodyMarkupSupported: true
        bodyHyperlinksSupported: true
        bodyImagesSupported: false
        actionIconsSupported: false
        imageSupported: false
        inlineReplySupported: false
        persistenceSupported: false

        onNotification: notification => root.trackNotification(notification)
    }

    Repeater {
        model: server.trackedNotifications

        delegate: Item {
            id: tracker

            required property var modelData

            Connections {
                target: tracker.modelData

                function onClosed(reason) {
                    root.removeNotification(tracker.modelData)
                }
            }

            Timer {
                interval: root.config.notificationToastTimeout
                repeat: false
                running: tracker.modelData.urgency !== NotificationUrgency.Critical
                onTriggered: tracker.modelData.expire()
            }
        }
    }

    Variants {
        model: Quickshell.screens

        Notifications.ToastHost {
            required property var modelData
            screen: modelData
            visible: modelData.name === root.config.primaryMonitor
            config: root.config
            controller: root
        }
    }
}
