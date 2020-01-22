package color

import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA

data class ColorTheme(val darkest: RGBA, val darker: RGBA, val base: RGBA, val lighter: RGBA, val lighteR: RGBA, val lightest: RGBA)

object KcgColors {
    val HUMAN_COLORS = ColorTheme(
            opaque(10, 90, 131),
            opaque(12, 105, 153),
            opaque(14, 123, 178),
            opaque(26, 145, 207),
            opaque(38, 160, 223),
            opaque(50, 178, 245))
    val ORC_COLORS = ColorTheme(
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED)
    val NEUTRAL_COLORS = ColorTheme(
            opaque(123, 120, 120),
            opaque(137, 133, 133),
            opaque(150, 146, 146),
            opaque(164, 160, 160),
            opaque(172, 168, 168),
            opaque(178, 175, 175))
}

fun opaque(r: Int, g: Int, b: Int) : RGBA { return RGBA.unclamped(r, g, b, 255) }
