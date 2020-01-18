package creatures

import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA

data class TeamColor(val darkest: RGBA, val darker: RGBA, val base: RGBA, val lighter: RGBA, val lighteR: RGBA, val lightest: RGBA)

object TeamColors {
    val HUMAN_COLORS = TeamColor(
            opaque(10, 90, 131),
            opaque(12, 105, 153),
            opaque(14, 123, 178),
            opaque(26, 145, 207),
            opaque(38, 160, 223),
            opaque(50, 178, 245))
    val ORC_COLORS = TeamColor(
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED)
}

fun opaque(r: Int, g: Int, b: Int) : RGBA { return RGBA.unclamped(r, g, b, 255) }
