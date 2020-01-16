package twod

import Direction
import com.soywiz.korge.view.Container
import com.soywiz.korge.view.position
import com.soywiz.korge.view.solidRect
import com.soywiz.korim.color.RGB
import com.soywiz.korim.color.RGBA
import com.soywiz.korma.geom.PointInt
import com.soywiz.korma.geom.RectangleInt
import kotlin.math.*

data class IntLine(val x: Int, val y: Int, val length: Int, val horizontalOrVertical: Boolean)

fun Container.solidIntLine(l: IntLine, color: RGBA) {
    if (l.horizontalOrVertical) solidRect(l.length, 1, color) { position(l.x, l.y) }
    else                        solidRect(1, l.length, color) { position(l.x, l.y) }
}

fun Container.solidInnerBorders(r: RectangleInt, color: RGBA) {
    r.innerBorders().forEach { this.solidIntLine(it, color) }
}

fun Container.solidIntRect(r: RectangleInt, color: RGBA) {
    solidRect(r.width, r.height, color) { position(r.x, r.y) }
}

fun Container.solidPoint(p : PointInt, color: RGBA) {
    solidRect(1, 1, color) { position(p.x, p.y) }
}

fun RectangleInt.shrink(): RectangleInt {
    return RectangleInt.invoke(x + 1, y + 1, max(0, width -2), max(0, height - 2))
}

fun RectangleInt.innerBorders(): List<IntLine> {
    val h = height
    val w = width
    return listOf<IntLine>(
            IntLine(x+1, y+1, max(0, w-2), true),   // top horizontal line
            IntLine(x+1, y+h-2, max(0, w-2), true), // bottom horizontal line
            IntLine(x+1, y+1, max(0, h-2), false),  // left vertical line
            IntLine(x+w-2, y+1, max(0, h-2), false) // right vertical line
    )
}

fun RectangleInt.corner(dir: Direction): PointInt {
    val offset = when (dir) {
        Direction.BOTTOM -> PointInt(width / 2, height)
        Direction.BOTTOM_RIGHT -> PointInt(width, height)
        Direction.BOTTOM_LEFT -> PointInt(0, height)
        Direction.LEFT -> PointInt(0, height / 2)
        Direction.RIGHT -> PointInt(width, height / 2)
        Direction.TOP -> PointInt(width / 2, height)
        Direction.TOP_LEFT -> PointInt(0, 0)
        Direction.TOP_RIGHT -> PointInt(width, 0)
    }
    return PointInt(x + offset.x,  y + offset.y)
}
