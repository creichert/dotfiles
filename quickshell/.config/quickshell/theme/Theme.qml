import QtQuick

QtObject {
    required property var config

    readonly property color surface: config.surfaceBaseColor
    readonly property color raisedSurface: config.surfaceRaisedColor
    readonly property color selectedSurface: config.surfaceSelectedColor
    readonly property color primaryText: config.textPrimaryColor
    readonly property color mutedText: config.textMutedColor
    readonly property color activeAccent: config.accentActiveColor
    readonly property color separator: config.separatorColor
    readonly property color urgent: config.urgentColor

    readonly property string fontFamily: config.fontFamily
    readonly property int fontPixelSize: config.fontPixelSize
    readonly property int titleFontPixelSize: config.isLaptop ? 16 : 18
    readonly property int bodyFontPixelSize: Math.max(14, config.fontPixelSize)
    readonly property int secondaryFontPixelSize: 12
    readonly property int surfaceRadius: config.surfaceRadius
    readonly property int controlRadius: config.controlRadius

    readonly property int panelPadding: 16
    readonly property int sectionSpacing: 16

    // Existing gaps and margins used across the shell.
    readonly property int spacingSmall: 4
    readonly property int spacingMedium: 8
    readonly property int spacingLarge: 12
}
