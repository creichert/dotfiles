import QtQuick
import "Icons.js" as Icons

Text {
    required property var theme
    required property string name

    text: Icons.glyph(name)
    color: theme.primaryText
    font.family: theme.fontFamily
    font.pixelSize: theme.fontPixelSize
}
