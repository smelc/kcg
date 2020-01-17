package card

import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korma.geom.RectangleInt
import creatures.Creature
import twod.*
import twod.Zone.Companion.solidZone

data class CardDrawingInput(val creature: Creature, val cbmp: BitmapSlice<Bitmap>, val font: BitmapFont, val tiles: Map<Tile, BitmapSlice<Bitmap>>)

const val creatureScale = 3.0
val backgroundColor = RGBA.unclamped(247, 232, 150, 255)

fun Stage.putBackground() {
    solidRect(width, height, backgroundColor)
}

fun Stage.putBorder(cdi: CardDrawingInput) {
    var rect: RectangleInt = RectangleInt.invoke(0, 0, width.toInt(), height.toInt())
    for (i in 0..4) {
        rect = rect.shrink()
        solidInnerBorders(rect, cdi.creature.team.color.base)
    }
}

fun Stage.putBorderDecoration(cdi: CardDrawingInput) {
    val rect: RectangleInt = RectangleInt.invoke(0, 0, width.toInt(), height.toInt()).shrink().shrink() // We need to shrink twice
    // to be at the topleft of the border, because the border is the canvas shrank once, and then the inner borders are taken
    rect.corners().forEach { solidPointInt(it, backgroundColor) }

    val lighterZones: MutableList<Zone> = mutableListOf()
    val lighterLen = (height / 8).toInt()
    lighterZones.add(rect.corner(Direction.TOP_LEFT).down().toLine(lighterLen, false)) // bar going down on the left + 1
    lighterZones.add(rect.corner(Direction.TOP_LEFT).right().toLine((lighterLen * 0.3).toInt(), true)) // bar going right
    lighterZones.forEach {  solidZone(it, cdi.creature.team.color.lighter) }

    val lighteRZones: MutableList<Zone> = mutableListOf()
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).down().toLine((lighterLen * 0.3).toInt(), false)) // bar going down on the left + 1
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).down().right().toLine((lighterLen * 0.05).toInt(), false)) // bar going down on the left + 1
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).right().toLine((lighterLen * 0.1).toInt(), true)) // bar going right one pixel below
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).right().down().toLine((lighterLen * 0.05).toInt(), true)) // bar going right one pixel below
    lighteRZones.forEach {  solidZone(it, cdi.creature.team.color.lighteR) }

    solidPointInt(rect.corner(Direction.TOP_LEFT).plusx(1), cdi.creature.team.color.lightest)
}

/** @return The tile's bottom y */
fun Stage.putCreatureTile(cdi: CardDrawingInput): Double {
    val imgx = (width - (cdi.cbmp.width * creatureScale)) / 2
    val imgy = (height - (cdi.cbmp.height * creatureScale)) / 8
    val img = image(cdi.cbmp) {
        position(imgx, imgy)
        scale = creatureScale
        smoothing = false
    }
    return imgy + img.height
}

/**
 * @param tileboty The creature's tile bottom y
 * @return The text's y
 */
fun Stage.putCreatureName(cdi: CardDrawingInput, tileboty: Double): Double {
    val texty = tileboty
    val w = width

    text(cdi.creature.name, font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = cdi.creature.team.color.base) {
        position((w - textBounds.width) / 2, texty)
    }

    return texty
}

/**
 * @param texty The creature's name y
 */
fun Stage.putStats(cdi: CardDrawingInput, texty: Double) {
    val hearty = texty + cdi.font.fontSize * 1.5
    val leftMargin = cdi.font.fontSize

    /* Hitpoints */
    val hpText = text (cdi.creature.hps.toString(), font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = Colors.BLACK) {
        position(leftMargin, hearty)
    }
    image(cdi.tiles[Tile.HEART] ?: error("heart tile not found")) {
        position(leftMargin + hpText.textBounds.width, hearty)
        scale = creatureScale - 1
        smoothing = false
    }

    /* Attack */
    val attacky = hearty + cdi.font.fontSize
    val attackText = text (cdi.creature.attack.toString(), font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = Colors.BLACK) {
        position(leftMargin, attacky)
    }
    image(cdi.tiles[Tile.SWORD] ?: error("sword tile not found")) {
        position(leftMargin + attackText.textBounds.width, attacky)
        scale = creatureScale - 1
        smoothing = false
    }
}
