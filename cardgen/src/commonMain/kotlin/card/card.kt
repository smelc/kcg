package card

import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korma.geom.RectangleInt
import creatures.Creature
import twod.Tile
import twod.shrink
import twod.solidInnerBorders

data class CardDrawingInput(val creature: Creature, val cbmp: BitmapSlice<Bitmap>, val font: BitmapFont, val tiles: Map<Tile, BitmapSlice<Bitmap>>)

val creatureScale = 3.0

fun Stage.putBackground(cdi: CardDrawingInput) {
    val background: RGBA = RGBA.unclamped(247, 232, 150, 255)
    solidRect(width, height, background)
}

fun Stage.putBorder(cdi: CardDrawingInput) {
    var rect: RectangleInt = RectangleInt.invoke(0, 0, width.toInt(), height.toInt())
    for (i in 0..4) {
        rect = rect.shrink()
        solidInnerBorders(rect, cdi.creature.team.color)
    }
}

/** @return The tile's y */
fun Stage.putCreatureTile(cdi: CardDrawingInput): Double {
    val imgx = (width - (cdi.cbmp.width * creatureScale)) / 2;
    val imgxCenter = imgx + (cdi.cbmp.width / 2)
    val imgy = (height - (cdi.cbmp.height * creatureScale)) / 8;
    val imgyCenter = imgy + (cdi.cbmp.height / 2)
    image(cdi.cbmp) {
        position(imgx, imgy)
        scale = creatureScale
        smoothing = false
    }
    return imgy
}

/**
 * @param tiley The creature's tile y
 * @return The text's y
 */
fun Stage.putCreatureName(cdi: CardDrawingInput, tiley: Double): Double {
    val texty = tiley + cdi.cbmp.height / 2 + (cdi.font.fontSize * 2)
    val w = width

    text(cdi.creature.name, font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = cdi.creature.team.color) {
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
