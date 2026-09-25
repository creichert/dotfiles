pragma ComponentBehavior: Bound

// qmllint disable signal-handler-parameters

import QtQuick
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io

Item {
    id: root

    required property var config
    property bool pickerVisible: false
    property bool loading: false
    property bool previewLoading: false
    property bool previewTruncated: false
    property string query: ""
    property string errorMessage: ""
    property string previewText: ""
    property string previewImageSource: ""
    property string previewEntryId: ""
    property int currentIndex: 0
    property var entries: []
    property var results: []
    readonly property var currentEntry: currentIndex >= 0 && currentIndex < results.length
        ? results[currentIndex] : null
    readonly property var targetScreen: screenForFocusedMonitor()

    function normalize(value) {
        return String(value || "").trim().toLocaleLowerCase()
    }

    function entryFromLine(line) {
        const separator = line.indexOf("\t")

        if (separator <= 0)
            return null

        const id = line.slice(0, separator)
        const preview = line.slice(separator + 1)

        if (!/^\d+$/.test(id))
            return null

        // cliphist 0.7.x exposes image metadata only through this preview text.
        const binary = preview.match(/^\[\[ binary data (.+) ([A-Za-z0-9]+) (\d+x\d+) \]\]$/)

        if (binary) {
            const format = binary[2].toLocaleLowerCase()
            const details = `${format.toLocaleUpperCase()} | ${binary[3]} | ${binary[1]}`

            return {
                id: id,
                kind: "image",
                preview: preview,
                title: "Image",
                details: details,
                format: format,
                searchText: normalize(`image ${details}`)
            }
        }

        if (preview.startsWith("[[ binary data ")) {
            return {
                id: id,
                kind: "unsupported",
                preview: preview,
                title: "Unsupported binary entry",
                details: "Unknown cliphist preview format",
                format: "",
                searchText: normalize(`unsupported binary ${preview}`)
            }
        }

        return {
            id: id,
            kind: "text",
            preview: preview,
            title: preview,
            details: "Text",
            format: "",
            searchText: normalize(`text ${preview}`)
        }
    }

    function parseEntries(output) {
        const next = []
        let invalidEntries = 0

        for (const line of output.split("\n")) {
            if (line.length === 0)
                continue

            const entry = entryFromLine(line)

            if (entry)
                next.push(entry)
            else
                invalidEntries++
        }

        entries = next
        rebuildResults()
        loading = false

        if (invalidEntries > 0)
            errorMessage = "Unsupported cliphist list format"
    }

    function rebuildResults() {
        const normalizedQuery = normalize(query)
        const tokens = normalizedQuery.length > 0 ? normalizedQuery.split(/\s+/) : []
        const next = entries.filter(entry =>
            tokens.every(token => entry.searchText.includes(token)))

        results = next
        currentIndex = Math.min(currentIndex, Math.max(0, next.length - 1))
        refreshPreview()
    }

    function setQuery(value) {
        const nextQuery = String(value || "")

        if (query === nextQuery)
            return

        query = nextQuery
        currentIndex = 0
        rebuildResults()
    }

    function move(delta) {
        if (results.length === 0)
            return

        currentIndex = Math.max(0, Math.min(results.length - 1, currentIndex + delta))
        refreshPreview()
    }

    function select(index) {
        if (index < 0 || index >= results.length || currentIndex === index)
            return

        currentIndex = index
        refreshPreview()
    }

    function imageDirectory() {
        const runtimeDirectory = Quickshell.env("XDG_RUNTIME_DIR") || "/tmp"
        return `${runtimeDirectory}/quickshell-cliphist`
    }

    function imagePath(entry) {
        return `${imageDirectory()}/preview-${entry.id}.${entry.format}`
    }

    function refreshPreview() {
        const entry = currentEntry

        previewDelay.stop()
        previewProcess.running = false
        previewEntryId = entry ? entry.id : ""
        previewText = ""
        previewImageSource = ""
        previewTruncated = false
        previewLoading = entry !== null
        errorMessage = ""

        if (!entry) {
            previewLoading = false
            return
        }

        previewDelay.restart()
    }

    function loadPreview() {
        const entry = currentEntry

        if (!entry || entry.id !== previewEntryId)
            return

        previewProcess.requestedEntryId = entry.id
        previewProcess.requestedImage = entry.kind === "image"
        previewProcess.requestedImagePath = entry.kind === "image" ? imagePath(entry) : ""

        if (entry.kind === "unsupported") {
            previewLoading = false
            errorMessage = "Unsupported cliphist binary preview format"
            return
        }

        if (entry.kind === "image") {
            previewProcess.exec([
                "bash",
                "-c",
                "mkdir -p \"$2\" && rm -f \"$2\"/preview \"$2\"/preview-* && cliphist decode \"$1\" > \"$3\"",
                "clipboard-preview",
                entry.id,
                imageDirectory(),
                previewProcess.requestedImagePath
            ])
        } else {
            previewProcess.exec([
                "bash",
                "-c",
                "cliphist decode \"$1\" | dd iflag=fullblock bs=32001 count=1 status=none; status=${PIPESTATUS[0]}; [[ $status -eq 0 || $status -eq 141 ]]",
                "clipboard-preview",
                entry.id
            ])
        }
    }

    function accept() {
        const entry = currentEntry

        if (!entry || restoreProcess.running)
            return

        errorMessage = ""
        restoreProcess.exec([
            "bash",
            "-o",
            "pipefail",
            "-c",
            "cliphist decode \"$1\" | wl-copy",
            "clipboard-restore",
            entry.id
        ])
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
        currentIndex = 0
        entries = []
        results = []
        errorMessage = ""
        previewEntryId = ""
        previewText = ""
        previewImageSource = ""
        previewTruncated = false
        previewLoading = false
        loading = true
        pickerVisible = true
        listProcess.exec(["cliphist", "-preview-width", "500", "list"])
    }

    function close() {
        listProcess.running = false
        previewDelay.stop()
        previewProcess.running = false
        pickerVisible = false
    }

    function toggle() {
        if (pickerVisible)
            close()
        else
            open()
    }

    Process {
        id: listProcess

        stdout: StdioCollector {
            onStreamFinished: root.parseEntries(text)
        }

        onExited: exitCode => {
            if (exitCode !== 0 && root.pickerVisible) {
                root.loading = false
                root.errorMessage = "Could not load clipboard history"
            }
        }
    }

    Process {
        id: previewProcess

        property string requestedEntryId: ""
        property bool requestedImage: false
        property string requestedImagePath: ""

        stdout: StdioCollector {
            onStreamFinished: {
                if (previewProcess.requestedImage
                        || previewProcess.requestedEntryId !== root.previewEntryId)
                    return

                root.previewTruncated = text.length > root.config.clipboardPreviewMaximumCharacters
                root.previewText = text.slice(0, root.config.clipboardPreviewMaximumCharacters)
                root.previewLoading = false
            }
        }

        onExited: exitCode => {
            if (requestedEntryId !== root.previewEntryId)
                return

            if (exitCode !== 0) {
                root.previewLoading = false
                root.errorMessage = "Could not preview clipboard entry"
            } else if (requestedImage) {
                root.previewImageSource = "file://" + requestedImagePath
                root.previewLoading = false
            }
        }
    }

    Timer {
        id: previewDelay

        interval: 75
        repeat: false
        onTriggered: root.loadPreview()
    }

    Process {
        id: restoreProcess

        onExited: exitCode => {
            if (exitCode === 0)
                root.close()
            else
                root.errorMessage = "Could not restore clipboard entry"
        }
    }

    IpcHandler {
        target: "clipboard"

        function togglePicker(): void {
            root.toggle()
        }
    }
}
