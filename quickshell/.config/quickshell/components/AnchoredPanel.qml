// qmllint disable uncreatable-type

import QtQuick
import Quickshell

PopupWindow {
    id: root

    required property Item trigger
    property int gap: 0

    color: "transparent"
    grabFocus: true

    // Quickshell's generated qmltypes omit these anchor flag types.
    // qmllint disable missing-type
    anchor {
        item: root.trigger
        edges: Edges.Bottom
        gravity: Edges.Bottom
        margins.bottom: root.gap
        adjustment: PopupAdjustment.SlideX | PopupAdjustment.ResizeY
    }
    // qmllint enable missing-type
}
