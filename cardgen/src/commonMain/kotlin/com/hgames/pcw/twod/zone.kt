package com.hgames.pcw.twod

import com.hgames.pcw.KorgeCandidate
import com.soywiz.korge.view.Stage
import com.soywiz.korim.color.RGBA
import com.soywiz.korma.geom.PointInt
import com.soywiz.korma.geom.RectangleInt

/**
 * A list of points, be it an area of pixels or cells in a 2D dungeon for example.
 *
 * <p>
 * The [toList] method is in the interface, for subclassers that wanna define [get] in terms of [toList]
 * </p>
 *
 * @author smelc
 */
@KorgeCandidate
interface Zone {

    fun get() : Iterable<PointInt>
    fun toList(): List<PointInt> = get().toList()

    companion object {
        fun Stage.solidZone(z: Zone, color: RGBA) {
            // TODO smelc Optimize for some Zone subtypes
            z.get().forEach { solidPointInt(it, color) }
        }

        fun Stage.solidZones(zones:  Iterable<Zone>, color: RGBA) {
            zones.forEach { solidZone(it, color) }
        }
    }

}

fun RectangleInt.toZone(): Zone {
    val l: MutableList<PointInt> = mutableListOf()
    for (px in x .. x + width) {
        for (py in y .. y + height)
            l.add(PointInt(px, py))
    }
    val immutList: List<PointInt> = l.toList()
    return object: Zone {
        override fun get(): Iterable<PointInt> { return toList() }
        override fun toList(): List<PointInt> { return immutList }
    }
}

fun PointInt.toZone(): Zone {
    val l = listOf(this)
    return object: Zone {
        override fun get(): Iterable<PointInt> { return toList() }
        override fun toList(): List<PointInt> { return l }
    }
}
