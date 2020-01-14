package twod

import com.soywiz.korge.view.Container
import com.soywiz.korge.view.position
import com.soywiz.korge.view.solidRect
import com.soywiz.korim.color.RGBA
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

fun RectangleInt.shrink(): RectangleInt {
    return RectangleInt.invoke(x + 1, y +1, max(0, width -2), max(0, height - 2))
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
