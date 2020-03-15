package com.hgames.pcw

import Direction
import com.hgames.pcw.KcgColors.NEUTRAL_COLORS
import com.hgames.pcw.twod.*
import com.hgames.pcw.twod.Zone.Companion.solidZones
import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korma.geom.RectangleInt

interface ICard {

    val name: String
    val title: String
    fun getBitmap(): BitmapSlice<Bitmap>
    fun getColorTheme(): ColorTheme

}

class CreatureCard(val creature: Creature, private val bmp: BitmapSlice<Bitmap>) : ICard {
    override val name: String = creature.name
    override val title: String = creature.title
    override fun getBitmap(): BitmapSlice<Bitmap> {
        return bmp
    }

    override fun getColorTheme(): ColorTheme {
        return creature.team.color
    }
}

data class CardDrawingInput(val card: ICard, val font: BitmapFont, val itfont: BitmapFont, val tiles: Map<Tile, BitmapSlice<Bitmap>>, val skills: List<SkillData>)

val backgroundColor = RGBA.unclamped(247, 232, 150, 255)
const val borderSize = 5

fun getVerticalSpaceAfterTitle(cdi: CardDrawingInput): Double {
    return cdi.font.fontSize * 1.5
}

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
    darkestZones.add(darkestRect.toZone())
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
    val imgx = (width - (bmp.width)) / 2
    val imgy = (height - (bmp.height)) / 12
    val img = image(bmp) {
        position(imgx, imgy)
        // smoothing = false
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

fun Stage.putJustifiedText(cdi: CardDrawingInput, text: String, basey: Double) {
    val leftMargin = getLeftMargin(cdi).toDouble()
   putJustifiedText(text, cdi.font, NEUTRAL_COLORS.darkesT, leftMargin, basey,  (width - leftMargin * 2))
}

/**
 * @param texty The creature's name y
 */
fun Stage.putStats(cdi: CardDrawingInput, card: CreatureCard, texty: Double) {
    val leftMargin = getLeftMargin(cdi)
    val statsLineY = texty + getVerticalSpaceAfterTitle(cdi)
    var skilly = putStatsLine(cdi, card, leftMargin, statsLineY)

    /* Skills */
    // val gold: RGBA = opaque(197, 195, 44)
    val gold: RGBA = opaque(164, 163, 30)
    val grey: RGBA = opaqueGrey(1118)
    for (skill in card.creature.skills) {
        val found: SkillData = cdi.skills.find { it.skill == skill }
                ?: throw IllegalStateException("SkillData not found: $skill")
        val bucket1 = TextBucket(found.title, cdi.font, gold)
        val bucket2 = TextBucket(found.text , cdi.itfont, grey)
        val consumedHeight = putJustifiedText2(bucket1, bucket2, leftMargin.toDouble(), skilly, (width - leftMargin * 2))
        skilly += consumedHeight + cdi.itfont.lineHeight
    }
}

/**
 * @param xbase Where to start displaying
 * @param ybase Where to start displaying
 * @return The y for the content after the content drawn by this call
 */
fun Stage.putStatsLine(cdi: CardDrawingInput, card: CreatureCard, xbase: Int, ybase: Double): Double {
    /* Hitpoints */
    val hpText = text(card.creature.hps.toString(),
            font = cdi.font, textSize = cdi.font.fontSize.toDouble(),
            color = opaque(254, 1, 1)) {
        position(xbase, ybase)
    }
    val hpImg = image(cdi.tiles[Tile.HEART] ?: error("heart tile not found")) {
        position(xbase + hpText.textBounds.width, ybase)
        smoothing = false
    }

    /* Attack */
    val attackx = hpImg.x + hpImg.width + cdi.font.measureWidth(" ")
    val attackText = text(card.creature.attack.toString(),
            font = cdi.font, textSize = cdi.font.fontSize.toDouble(),
            color = opaque(66, 57, 23)) {
        position(attackx, ybase)
    }
    image(cdi.tiles[Tile.SWORD] ?: error("sword tile not found")) {
        position(attackx + attackText.textBounds.width, ybase)
        smoothing = false
    }

    val ySeparator = cdi.font.fontSize * 1.3
    return ybase + ySeparator
}

private fun getLeftMargin(cdi: CardDrawingInput): Int {
    return cdi.font.fontSize
}
