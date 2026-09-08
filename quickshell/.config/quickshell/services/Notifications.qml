pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications
import "../notifications" as Notifications

Item {
    id: root

    required property var config
    signal interacted()
    property alias visibleNotifications: state.visibleNotifications
    property alias queuedNotifications: state.queuedNotifications
    property alias lastDismissed: state.lastDismissed
    property alias history: state.history
    property alias doNotDisturb: state.doNotDisturb
    property bool notificationCenterVisible: false
    property var liveNotifications: []
    property var actionNotifications: []

    PersistentProperties {
        id: state

        reloadableId: "notificationController"
        property var visibleNotifications: []
        property var queuedNotifications: []
        property var lastDismissed: null
        property var history: []
        property bool doNotDisturb: false
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
        liveNotifications = removeFrom(liveNotifications, notification)
        releaseActions(notification.id)

        if (lastDismissed && lastDismissed.id === notification.id)
            lastDismissed = null

        showQueuedNotifications()
    }

    function recordNotification(notification) {
        const record = {
            id: notification.id,
            appName: notification.appName,
            appIcon: notification.appIcon,
            desktopEntry: notification.desktopEntry,
            summary: notification.summary,
            body: notification.body,
            urgency: notification.urgency,
            timestamp: Date.now(),
            unread: true,
            hasActions: notification.actions.length > 0,
            actionButtonsExpireAt: notification.expireTimeout > 0
                ? Date.now() + notification.expireTimeout * 1000
                : 0
        }
        const index = history.findIndex(existing => existing.id === notification.id)
        const next = history.slice()

        if (index === -1)
            next.push(record)
        else
            next[index] = record

        if (next.length > config.notificationHistoryLimit) {
            const removed = next.splice(0, next.length - config.notificationHistoryLimit)

            for (const previous of removed) {
                const notification = notificationById(liveNotifications, previous.id)

                if (notification)
                    notification.dismiss()
                releaseActions(previous.id)
            }
        }

        history = next
    }

    function notificationById(notifications, id) {
        for (const notification of notifications) {
            if (notification.id === id)
                return notification
        }

        return null
    }

    function actionNotification(id) {
        return notificationById(actionNotifications, id)
    }

    function retainActions(notification) {
        if (notification.actions.length === 0)
            return

        actionNotifications = removeFrom(actionNotifications, notification).concat(notification)
    }

    function releaseActions(id) {
        const notification = actionNotification(id)

        if (notification)
            actionNotifications = removeFrom(actionNotifications, notification)
    }

    function historyRecord(id) {
        for (const record of history) {
            if (record.id === id)
                return record
        }

        return null
    }

    function updateHistory(id, update) {
        const index = history.findIndex(record => record.id === id)

        if (index === -1)
            return

        const next = history.slice()
        next[index] = Object.assign({}, next[index], update)
        history = next
    }

    function setRead(id, unread) {
        updateHistory(id, { unread: unread })
    }

    function dismissHistoryRecord(id) {
        const notification = notificationById(liveNotifications, id)

        if (notification)
            notification.dismiss()

        removeHistoryRecord(id)
    }

    function removeHistoryRecord(id) {
        history = history.filter(record => record.id !== id)
        releaseActions(id)
    }

    function clearHistory() {
        const notifications = liveNotifications.slice()

        visibleNotifications = []
        queuedNotifications = []
        liveNotifications = []
        actionNotifications = []
        lastDismissed = null
        history = []

        for (const notification of notifications)
            notification.dismiss()
    }

    function actionsFor(id) {
        const notification = actionNotification(id)
            || notificationById(liveNotifications, id)

        return notification ? notification.actions : []
    }

    function nonDefaultActionsFor(id) {
        return actionsFor(id).filter(action => action.identifier !== "default")
    }

    function invokeAction(id, identifier) {
        for (const action of actionsFor(id)) {
            if (action.identifier === identifier) {
                action.invoke()
                removeHistoryRecord(id)
                interacted()
                return true
            }
        }

        return false
    }

    function invokeDefaultAction(id) {
        return invokeAction(id, "default")
    }

    function notificationInteracted() {
        interacted()
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
        liveNotifications = removeFrom(liveNotifications, notification).concat(notification)
        recordNotification(notification)
        releaseActions(notification.id)
        retainActions(notification)

        // Replacement notifications reuse an ID and keep their current slot.
        const wasVisible = indexOfNotification(visibleNotifications, notification) !== -1
        visibleNotifications = removeFrom(visibleNotifications, notification)
        queuedNotifications = removeFrom(queuedNotifications, notification)

        if (wasVisible)
            visibleNotifications = visibleNotifications.concat(notification)
        else if (!doNotDisturb || notification.urgency === NotificationUrgency.Critical)
            showNotification(notification)
    }

    function hideVisibleNotifications() {
        if (visibleNotifications.length === 0)
            return

        lastDismissed = visibleNotifications[visibleNotifications.length - 1]
        visibleNotifications = []
        showQueuedNotifications()
    }

    function hideNotification(notification) {
        visibleNotifications = removeFrom(visibleNotifications, notification)
        queuedNotifications = removeFrom(queuedNotifications, notification)
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
        removeHistoryRecord(notification.id)
    }

    IpcHandler {
        target: "notifications"

        function dismissVisible(): void {
            root.hideVisibleNotifications()
        }

        function restoreLastDismissed(): void {
            root.restoreLastDismissed()
        }

        function toggleDoNotDisturb(): void {
            root.doNotDisturb = !root.doNotDisturb
        }

        function toggleNotificationCenter(): void {
            root.notificationCenterVisible = !root.notificationCenterVisible
        }

        function clearHistory(): void {
            root.clearHistory()
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
                // Keep history actions valid after the toast leaves the screen.
                onTriggered: root.hideNotification(tracker.modelData)
            }
        }
    }

    Instantiator {
        model: root.actionNotifications

        delegate: RetainableLock {
            required property var modelData

            object: modelData
            locked: true
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
