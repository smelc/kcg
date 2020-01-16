package creatures

import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA

data class TeamColor(val base: RGBA, val lighter: RGBA, val lighteR: RGBA, val lightest: RGBA)

object TeamColors {
    val HUMAN_COLORS = TeamColor(
            RGBA.unclamped(14, 123, 178, 255),
            RGBA.unclamped(26, 145, 207, 255),
            RGBA.unclamped(38, 160, 223, 255),
            RGBA.unclamped(50, 178, 245, 255))
    val ORC_COLORS = TeamColor(
            Colors.RED,
            Colors.RED,
            Colors.RED,
            Colors.RED)
}

