package com.hgames.pcw.twod

import Direction
import com.hgames.pcw.KorgeCandidate
import com.soywiz.korge.view.Container
import com.soywiz.korge.view.position
import com.soywiz.korge.view.solidRect
import com.soywiz.korim.color.RGBA
import com.soywiz.korma.geom.PointInt
import com.soywiz.korma.geom.RectangleInt
import kotlin.math.*

@KorgeCandidate
data class LineInt(val x: Int, val y: Int, val length: Int, val horizontalOrVertical: Boolean) : Zone {

    fun toRectangleInt(): RectangleInt {
        return if (horizontalOrVertical) RectangleInt(x, y, length, 1)
               else RectangleInt(x, y, 1, length)
    }

    fun extend(len: Int): LineInt { return LineInt(x, y, length + len, horizontalOrVertical) }
    fun moveByX(amount: Int): LineInt = LineInt(x + amount, y, length, horizontalOrVertical)
    fun moveByY(amount: Int): LineInt = LineInt(x, y + amount, length, horizontalOrVertical)
    val shrink: LineInt by lazy { LineInt(x, y, max(0, length - 1), horizontalOrVertical) }
    fun growRectangleInt(growAmount: Int): RectangleInt {
        return if (horizontalOrVertical) RectangleInt(x, y, length, growAmount)
        else RectangleInt(x, y, growAmount, length)
    }

    override fun get(): Iterable<PointInt> {
        val ml = mutableListOf<PointInt>()
        for (inc in 0..length)
            ml.add(PointInt(x + if (horizontalOrVertical) inc else 0, y + if (horizontalOrVertical) 0 else inc))
        return ml.toList()
    }

    companion object {
        // operator fun invoke(x: Int, y: Int, length: Int, horizontalOrVertical: Boolean): LineInt = LineInt(x, y, length, horizontalOrVertical)
    }
}

@KorgeCandidate
fun PointInt.toLine(len: Int, horizontalOrVertical: Boolean): LineInt {
    if (len > 0) return LineInt(x, y, len, horizontalOrVertical)
    else if (horizontalOrVertical) return LineInt(x + len, y, -len, horizontalOrVertical)
    else return LineInt(x, y + len, -len, horizontalOrVertical)
}

@KorgeCandidate
fun PointInt.plusx(plus: Int): PointInt { return PointInt(x + plus, y) }
fun PointInt.left(): PointInt { return plusx(-1) }
fun PointInt.right(): PointInt { return plusx(1) }
@KorgeCandidate
fun PointInt.plusy(plus: Int): PointInt { return PointInt(x, y + plus) }
fun PointInt.up(): PointInt { return plusy(-1) }
fun PointInt.up(amount: Int): PointInt { return plusy(-amount) }
fun PointInt.down(): PointInt { return plusy(1) }
fun PointInt.down(amount: Int): PointInt { return plusy(amount) }

fun Container.solidInnerBorders(r: RectangleInt, color: RGBA) {
    r.innerBorders().forEach { this.solidLineInt(it, color) }
}

@KorgeCandidate
fun Container.solidLineInt(l: LineInt, color: RGBA) {
    solidRectangleInt(l.toRectangleInt(), color)
}

@KorgeCandidate
fun Container.solidPointInt(p : PointInt, color: RGBA) {
    solidRect(1, 1, color) { position(p.x, p.y) }
}

@KorgeCandidate
fun Container.solidRectangleInt(r: RectangleInt, color: RGBA) {
    solidRect(r.width, r.height, color) { position(r.x, r.y) }
}

@KorgeCandidate
fun RectangleInt.shrink(): RectangleInt {
    return RectangleInt.invoke(x + 1, y + 1, max(0, width -2), max(0, height - 2))
}

@KorgeCandidate
fun RectangleInt.corners(): List<PointInt> {
    return listOf(corner(Direction.TOP_LEFT), corner(Direction.BOTTOM_LEFT), corner(Direction.BOTTOM_RIGHT), corner(Direction.TOP_RIGHT))
}

fun RectangleInt.innerBorders(): List<LineInt> {
    val h = height
    val w = width
    return listOf<LineInt>(
            LineInt(x+1, y+1, max(0, w-2), true),   // top horizontal line
            LineInt(x+1, y+h-2, max(0, w-2), true), // bottom horizontal line
            LineInt(x+1, y+1, max(0, h-2), false),  // left vertical line
            LineInt(x+w-2, y+1, max(0, h-2), false) // right vertical line
    )
}

/**
 * @param dir The corner to retrieve
 * @result The corner (within the rectangle) at the given direction
 */
@KorgeCandidate
fun RectangleInt.corner(dir: Direction): PointInt {
    val offset = when (dir) {
        Direction.BOTTOM -> PointInt(width / 2, height - 1)
        Direction.BOTTOM_RIGHT -> PointInt(width - 1, height - 1)
        Direction.BOTTOM_LEFT -> PointInt(0, height - 1)
        Direction.LEFT -> PointInt(0, height / 2)
        Direction.RIGHT -> PointInt(width - 1, height / 2)
        Direction.TOP -> PointInt(width / 2, height - 1)
        Direction.TOP_LEFT -> PointInt(0, 0)
        Direction.TOP_RIGHT -> PointInt(width - 1, 0)
    }
    return PointInt(x + offset.x,  y + offset.y)
}
