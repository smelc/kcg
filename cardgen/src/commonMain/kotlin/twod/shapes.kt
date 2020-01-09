package twod

import com.soywiz.korge.view.Container
import com.soywiz.korge.view.position
import com.soywiz.korge.view.solidRect
import com.soywiz.korim.color.RGBA
import kotlin.math.*

data class IntRect(val x: Int, val y: Int, val w: Int, val h: Int) {
    /** @return A rectangle whose border has been removed */
    inline fun shrink(): IntRect {
        return IntRect(x + 1, y + 1, max(0, w-2), max(0, h-2))
    }

    fun innerBorders(): List<IntLine> {
        return listOf<IntLine>(
                IntLine(x+1, y+1, max(0, w-2), true), // top horizontal line
                IntLine(x+1, y+h-2, max(0, w-2), true), // bottom horizontal line
                IntLine(x+1, y+1, max(0, h-2), false), // left vertical line
                IntLine(x+w-2, y+1, max(0, h-2), false) // right vertical line
        )
    }
}

fun Container.solidIntRect(r: IntRect, color: RGBA) {
    solidRect(r.w, r.h, color) { position(r.x, r.y) }
}

data class IntLine(val x: Int, val y: Int, val length: Int, val horizontalOrVertical: Boolean)

fun Container.solidIntLine(l: IntLine, color: RGBA) {
    if (l.horizontalOrVertical) solidRect(l.length, 1, color) { position(l.x, l.y) }
    else                        solidRect(1, l.length, color) { position(l.x, l.y) }
}

fun Container.solidInnerBorders(r: IntRect, color: RGBA) {
    r.innerBorders().forEach { this.solidIntLine(it, color) }
}



