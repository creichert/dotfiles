pragma ComponentBehavior: Bound

import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io

Item {
    id: root

    required property var config
    property bool launcherVisible: false
    property string query: ""
    property string expandedEntryId: ""
    property int currentIndex: 0
    property var entries: []
    property var results: []
    property var launchCounts: ({})
    readonly property var currentResult: currentIndex >= 0 && currentIndex < results.length
        ? results[currentIndex] : null
    readonly property var targetScreen: screenForFocusedMonitor()

    function normalize(value) {
        return String(value || "").trim().toLocaleLowerCase()
    }

    function entrySubtitle(entry) {
        return entry.genericName.length > 0 ? entry.genericName : entry.comment
    }

    function resolvedIcon(icon) {
        return icon.length > 0
            ? Quickshell.iconPath(icon, "application-x-executable")
            : Quickshell.iconPath("application-x-executable", true)
    }

    function entryActions(entry) {
        const actions = []

        if (!config.launcherDesktopActionsEnabled)
            return actions

        for (const action of entry.actions) {
            if (action.id.length > 0)
                actions.push(action)
        }

        return actions
    }

    function entryRecord(entry) {
        const actions = entryActions(entry)
        const actionNames = actions.map(action => action.name).join(" ")
        const title = entry.name.length > 0 ? entry.name : entry.id
        const subtitle = entrySubtitle(entry)

        return {
            id: entry.id,
            title: title,
            subtitle: subtitle,
            iconSource: resolvedIcon(entry.icon),
            searchText: normalize([
                title,
                entry.genericName,
                entry.comment,
                entry.execString,
                entry.categories.join(" "),
                entry.keywords.join(" "),
                actionNames
            ].join(" ")),
            titleSearchText: normalize(title),
            actions: actions,
            entry: entry
        }
    }

    function isExcluded(entry) {
        for (const value of config.launcherExcludedEntries) {
            const excluded = normalize(value)

            if (excluded.length > 0 && (normalize(entry.id) === excluded
                    || normalize(entry.name).includes(excluded)))
                return true
        }

        return false
    }

    function rebuildEntries() {
        const next = []

        for (const entry of DesktopEntries.applications.values) {
            if (!isExcluded(entry))
                next.push(entryRecord(entry))
        }

        entries = next
        rebuildResults()
    }

    function queryScore(entry, tokens, normalizedQuery) {
        if (tokens.length === 0)
            return 0

        let score = 0

        for (const token of tokens) {
            if (!entry.searchText.includes(token))
                return -1

            if (entry.titleSearchText.startsWith(token))
                score += 100
            else if (entry.titleSearchText.includes(token))
                score += 60
            else
                score += 10
        }

        if (entry.titleSearchText === normalizedQuery)
            score += 1000
        else if (entry.titleSearchText.startsWith(normalizedQuery))
            score += 250

        return score
    }

    function rebuildResults(preferredId) {
        const normalizedQuery = normalize(query)
        const tokens = normalizedQuery.length > 0 ? normalizedQuery.split(/\s+/) : []
        const matches = []

        for (const entry of entries) {
            const score = queryScore(entry, tokens, normalizedQuery)

            if (score >= 0)
                matches.push({ entry: entry, score: score })
        }

        matches.sort((left, right) => {
            if (left.score !== right.score)
                return right.score - left.score

            const launchCountOrder = (launchCounts[right.entry.id] || 0)
                - (launchCounts[left.entry.id] || 0)
            if (launchCountOrder !== 0)
                return launchCountOrder

            const titleOrder = left.entry.title.localeCompare(right.entry.title)
            return titleOrder !== 0 ? titleOrder : left.entry.id.localeCompare(right.entry.id)
        })

        const next = []

        for (const match of matches) {
            const entry = match.entry
            next.push({
                id: entry.id,
                kind: "application",
                entry: entry
            })

            if (expandedEntryId === entry.id) {
                for (const action of entry.actions) {
                    next.push({
                        id: entry.id + ":" + action.id,
                        kind: "action",
                        entry: entry,
                        action: action
                    })
                }
            }
        }

        results = next

        if (preferredId) {
            const index = next.findIndex(result => result.id === preferredId)

            if (index !== -1) {
                currentIndex = index
                return
            }
        }

        currentIndex = Math.min(currentIndex, Math.max(0, next.length - 1))
    }

    function setQuery(value) {
        const nextQuery = String(value || "")

        if (query === nextQuery)
            return

        query = nextQuery
        expandedEntryId = ""
        currentIndex = 0
        rebuildResults()
    }

    function move(delta) {
        if (results.length === 0)
            return

        currentIndex = Math.max(0, Math.min(results.length - 1, currentIndex + delta))
    }

    function select(index) {
        if (index >= 0 && index < results.length)
            currentIndex = index
    }

    function toggleActions() {
        const result = currentResult

        if (!result)
            return

        const entryId = result.entry.id

        if (expandedEntryId === entryId) {
            expandedEntryId = ""
            rebuildResults(entryId)
            return
        }

        if (result.entry.actions.length === 0)
            return

        expandedEntryId = entryId
        rebuildResults(entryId)
    }

    function collapseActions() {
        if (expandedEntryId.length === 0)
            return false

        const entryId = expandedEntryId
        expandedEntryId = ""
        rebuildResults(entryId)
        return true
    }

    function launch(entry, action) {
        const prefix = config.launcherCommandPrefix

        if (prefix === null) {
            if (action)
                action.execute()
            else
                entry.execute()
        } else if (Array.isArray(prefix) && prefix.length > 0) {
            const desktopId = entry.id.endsWith(".desktop") ? entry.id : entry.id + ".desktop"
            const actionId = action ? action.id.trim() : ""
            const target = actionId.length > 0 ? desktopId + ":" + actionId : desktopId
            Quickshell.execDetached(prefix.concat(target))
        } else {
            console.error("launcherCommandPrefix must be null or a nonempty command array")
            return
        }

        const nextLaunchCounts = Object.assign({}, launchCounts)
        nextLaunchCounts[entry.id] = (nextLaunchCounts[entry.id] || 0) + 1
        launchCounts = nextLaunchCounts
        launcherVisible = false
    }

    function accept() {
        const result = currentResult

        if (!result)
            return

        launch(result.entry, result.kind === "action" ? result.action : null)
    }

    function screenForFocusedMonitor() {
        const monitor = Hyprland.focusedMonitor

        for (const screen of Quickshell.screens) {
            if (Hyprland.monitorFor(screen) === monitor)
                return screen
        }

        return Quickshell.screens.length > 0 ? Quickshell.screens[0] : null
    }

    function open() {
        query = ""
        expandedEntryId = ""
        currentIndex = 0
        rebuildResults()
        launcherVisible = true
    }

    function close() {
        launcherVisible = false
    }

    function toggle() {
        if (launcherVisible)
            close()
        else
            open()
    }

    IpcHandler {
        target: "launcher"

        function toggleLauncher(): void {
            root.toggle()
        }
    }

    Connections {
        target: DesktopEntries

        function onApplicationsChanged() {
            root.rebuildEntries()
        }
    }

    Component.onCompleted: rebuildEntries()
}
