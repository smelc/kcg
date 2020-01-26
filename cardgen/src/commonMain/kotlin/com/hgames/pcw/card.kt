package com.hgames.pcw

import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korma.geom.RectangleInt
import com.hgames.pcw.twod.*
import com.hgames.pcw.twod.Zone.Companion.solidZones

interface ICard {

    val title: String
    fun getBitmap(): BitmapSlice<Bitmap>
    fun getColorTheme(): ColorTheme

}

class CreatureCard(val creature: Creature, private val bmp: BitmapSlice<Bitmap>) : ICard {
    override val title: String = creature.name
    override fun getBitmap(): BitmapSlice<Bitmap> { return bmp }
    override fun getColorTheme(): ColorTheme { return creature.team.color }
}

data class CardDrawingInput(val card: ICard, val font: BitmapFont, val tiles: Map<Tile, BitmapSlice<Bitmap>>)

const val creatureScale = 3.0
val backgroundColor = RGBA.unclamped(247, 232, 150, 255)
val borderSize = 5

fun Stage.putBackground() {
    solidRect(width, height, backgroundColor)
}

fun Stage.putBorder(cdi: CardDrawingInput) {
    val canvas: RectangleInt = RectangleInt.invoke(0, 0, width.toInt(), height.toInt())
    var rect = canvas
    for (i in 0 until borderSize) {
        rect = rect.shrink()
        solidInnerBorders(rect, cdi.card.getColorTheme().base)
    }
}

fun Stage.putBorderDecoration(cdi: CardDrawingInput) {
    val rect: RectangleInt = RectangleInt.invoke(0, 0, width.toInt(), height.toInt()).shrink().shrink() // We need to shrink twice
    // to be at the topleft of the border, because the border is the canvas shrank once, and then the inner borders are taken

    val theme = cdi.card.getColorTheme()

    val lighterZones: MutableList<Zone> = mutableListOf()
    val lighterLen = (height / 8).toInt()
    lighterZones.add(rect.corner(Direction.TOP_LEFT).down().toLine(lighterLen, false)) // bar going down on the left + 1
    lighterZones.add(rect.corner(Direction.TOP_LEFT).right().toLine((lighterLen * 0.3).toInt(), true)) // bar going right
    solidZones(lighterZones, theme.lighter)

    val lighteRZones: MutableList<Zone> = mutableListOf()
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).down().toLine((lighterLen * 0.3).toInt(), false)) // bar going down on the left + 1
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).down().right().toLine((lighterLen * 0.05).toInt(), false)) // bar going down on the left + 1
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).right().toLine((lighterLen * 0.1).toInt(), true)) // bar going right one pixel below
    lighteRZones.add(rect.corner(Direction.TOP_LEFT).right().down().toLine((lighterLen * 0.05).toInt(), true)) // bar going right one pixel below
    solidZones(lighteRZones, theme.lighteR)

    solidPointInt(rect.corner(Direction.TOP_LEFT).plusx(1), theme.lightest)

    val bottomRight = rect.corner(Direction.BOTTOM_RIGHT)
    val bottomLeft = rect.corner(Direction.BOTTOM_LEFT)

    val darkestZones: MutableList<Zone> = mutableListOf()
    val darkestRect = bottomLeft.toLine(bottomRight.x - bottomLeft.x + 1, true).growRectangleInt(-borderSize)
    darkestZones.add(darkestRect.toZone());
    val atopLeftCorner = darkestRect.corner(Direction.TOP_LEFT).up(1).toLine(borderSize, true).growRectangleInt(borderSize / 2)
    darkestZones.add(atopLeftCorner.toZone())
    val atopRightCorner = darkestRect.corner(Direction.TOP_RIGHT).up(2).toLine(-borderSize, true).growRectangleInt(borderSize / 2)
    darkestZones.add(atopRightCorner.toZone())
    solidZones(darkestZones, theme.darkest)

    val darkerZones: MutableList<Zone> = mutableListOf()
    darkerZones.add(atopLeftCorner.moveByY(-borderSize / 2).toZone())
    darkerZones.add(atopRightCorner.moveByY(-borderSize / 2).toZone())
    solidZones(darkerZones, theme.darker)

    solidPointInt(atopRightCorner.corner(Direction.TOP_RIGHT).up(), theme.darkest)

    val darkesTZones: MutableList<Zone> = mutableListOf()
    darkesTZones.add(bottomRight.up().toZone())
    darkesTZones.add(bottomRight.left().toLine(-(borderSize * 1.5).toInt(), true))
    solidZones(darkesTZones, theme.darkesT)

    rect.corners().forEach { solidPointInt(it, backgroundColor) }
}

/** @return The tile's bottom y */
fun Stage.putCreatureTile(cdi: CardDrawingInput): Double {
    val bmp = cdi.card.getBitmap()
    val imgx = (width - (bmp.width * creatureScale)) / 2
    val imgy = (height - (bmp.height * creatureScale)) / 8
    val img = image(bmp) {
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
fun Stage.putTitle(cdi: CardDrawingInput, tileboty: Double): Double {
    val texty = tileboty
    val w = width

    text(cdi.card.title, font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = cdi.card.getColorTheme().base) {
        position((w - textBounds.width) / 2, texty)
    }

    return texty
}

/**
 * @param texty The creature's name y
 */
fun Stage.putStats(cdi: CardDrawingInput, card: CreatureCard, texty: Double) {
    val hearty = texty + cdi.font.fontSize * 1.5
    val leftMargin = cdi.font.fontSize

    /* Hitpoints */
    val hpText = text (card.creature.hps.toString(), font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = Colors.BLACK) {
        position(leftMargin, hearty)
    }
    image(cdi.tiles[Tile.HEART] ?: error("heart tile not found")) {
        position(leftMargin + hpText.textBounds.width, hearty)
        scale = creatureScale - 1
        smoothing = false
    }

    /* Attack */
    val attacky = hearty + cdi.font.fontSize
    val attackText = text (card.creature.attack.toString(), font = cdi.font, textSize = cdi.font.fontSize.toDouble(), color = Colors.BLACK) {
        position(leftMargin, attacky)
    }
    image(cdi.tiles[Tile.SWORD] ?: error("sword tile not found")) {
        position(leftMargin + attackText.textBounds.width, attacky)
        scale = creatureScale - 1
        smoothing = false
    }
}
