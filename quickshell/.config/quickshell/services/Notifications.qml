pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Services.Notifications
import "../notifications" as Notifications

Item {
    id: root

    required property var config
    property var primaryScreen: null
    signal interacted()
    signal notificationUpdated(var notification)
    property alias visibleNotifications: state.visibleNotifications
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

    function regularVisibleCount(notifications) {
        let count = 0

        for (const notification of notifications) {
            if (notification.urgency !== NotificationUrgency.Critical)
                count++
        }

        return count
    }

    function sameToastContent(first, second) {
        return first.desktopEntry === second.desktopEntry
            && first.appName === second.appName
            && first.summary === second.summary
            && first.body === second.body
            && first.urgency === second.urgency
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
        liveNotifications = removeFrom(liveNotifications, notification)
        releaseActions(notification.id)

        if (lastDismissed && lastDismissed.id === notification.id)
            lastDismissed = null
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
            actionButtonsExpireAt: notification.expireTimeout > 0
                ? Date.now() + notification.expireTimeout
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

    function setRead(id, isUnread) {
        updateHistory(id, { unread: isUnread })
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
                // Release layer-shell focus before asking the client to activate
                // a window or workspace for its notification action.
                interacted()
                Qt.callLater(() => {
                    action.invoke()
                    removeHistoryRecord(id)
                })
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

        if (notification.urgency === NotificationUrgency.Critical) {
            visibleNotifications = visibleNotifications.concat(notification)
            return
        }

        if (config.notificationMaximumVisible <= 0)
            return

        const next = visibleNotifications.filter(existing =>
            existing.urgency === NotificationUrgency.Critical
                || !sameToastContent(existing, notification))

        if (regularVisibleCount(next) >= config.notificationMaximumVisible) {
            const oldestRegular = next.findIndex(existing =>
                existing.urgency !== NotificationUrgency.Critical)

            if (oldestRegular !== -1)
                next.splice(oldestRegular, 1)
        }

        next.push(notification)
        visibleNotifications = next
    }

    function refreshNotification(notification) {
        if (indexOfNotification(liveNotifications, notification) === -1)
            return

        recordNotification(notification)
        releaseActions(notification.id)
        retainActions(notification)

        visibleNotifications = removeFrom(visibleNotifications, notification)

        if (!doNotDisturb || notification.urgency === NotificationUrgency.Critical)
            showNotification(notification)

        if (lastDismissed && lastDismissed.id === notification.id)
            lastDismissed = null

        notificationUpdated(notification)
    }

    function trackNotification(notification) {
        notification.tracked = true
        liveNotifications = removeFrom(liveNotifications, notification).concat(notification)

        if (!notification.lastGeneration)
            recordNotification(notification)

        releaseActions(notification.id)
        retainActions(notification)

        // Replacement notifications reuse an ID and keep their current slot.
        const wasVisible = indexOfNotification(visibleNotifications, notification) !== -1
        visibleNotifications = removeFrom(visibleNotifications, notification)

        if (wasVisible)
            visibleNotifications = visibleNotifications.concat(notification)
        else if (!notification.lastGeneration
                && (!doNotDisturb || notification.urgency === NotificationUrgency.Critical))
            showNotification(notification)

        if (lastDismissed && lastDismissed.id === notification.id)
            lastDismissed = notification
    }

    function hideVisibleNotifications() {
        if (visibleNotifications.length === 0)
            return

        lastDismissed = visibleNotifications[visibleNotifications.length - 1]
        visibleNotifications = []
    }

    function hideNotification(notification) {
        visibleNotifications = removeFrom(visibleNotifications, notification)
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
            property bool refreshPending: false

            function scheduleRefresh() {
                if (refreshPending)
                    return

                refreshPending = true
                Qt.callLater(() => {
                    refreshPending = false

                    if (tracker.modelData && tracker.modelData.tracked)
                        root.refreshNotification(tracker.modelData)
                })
            }

            Connections {
                target: tracker.modelData

                function onClosed(reason) {
                    root.removeNotification(tracker.modelData)
                }

                function onExpireTimeoutChanged() {
                    tracker.scheduleRefresh()
                }

                function onAppNameChanged() {
                    tracker.scheduleRefresh()
                }

                function onAppIconChanged() {
                    tracker.scheduleRefresh()
                }

                function onSummaryChanged() {
                    tracker.scheduleRefresh()
                }

                function onBodyChanged() {
                    tracker.scheduleRefresh()
                }

                function onUrgencyChanged() {
                    tracker.scheduleRefresh()
                }

                function onActionsChanged() {
                    tracker.scheduleRefresh()
                }

                function onDesktopEntryChanged() {
                    tracker.scheduleRefresh()
                }
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
        model: root.primaryScreen ? [root.primaryScreen] : []

        Notifications.ToastHost {
            required property var modelData
            screen: modelData
            config: root.config
            controller: root
        }
    }
}
