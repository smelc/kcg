import com.soywiz.korge.*
import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.*
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korim.font.readBitmapFont
import com.soywiz.korim.format.*
import com.soywiz.korio.file.std.*
import com.soywiz.korma.geom.RectangleInt
import creatures.Creature
import twod.Tile
import twod.shrink
import twod.solidInnerBorders

suspend fun main() = Korge(width = (24 * 9), height = ((24 * 4) + 12) * 3, bgcolor = Colors["#2b2b2b"]) {
	val dataJson = resourcesVfs["data.json"]
	val tiles = Tile.loadFromDisk(dataJson, resourcesVfs["tiles.png"].readBitmap())
	val creatures = Creature.loadFromDisk(dataJson, resourcesVfs["creatures.png"].readBitmap())

	val font = resourcesVfs["romulus.fnt"].readBitmapFont()

	for ((i, p) in creatures.withIndex()) {
		prepareCard(p.first, p.second, font, tiles)

		val bmp = renderToBitmap(this.views)
		val path = "/tmp/demo${i}.png"
		bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
	}
}

/**
 * @param cbmp The bitmap of [creature]
 */
fun Stage.prepareCard(creature: Creature, cbmp: BitmapSlice<Bitmap>, font: BitmapFont, tiles: Map<Tile, BitmapSlice<Bitmap>>) {
	val h : Double = height
    val w = width

	val background: RGBA = RGBA.unclamped(247, 232, 150, 255)
	solidRect(w, h, background)

    var rect: RectangleInt = RectangleInt.invoke(0, 0, w.toInt(), h.toInt())
    for (i in 0..4) {
        rect = rect.shrink()
		solidInnerBorders(rect, creature.team.color)
	}

	val creatureScale = 3.0
	val imgx = (w - (cbmp.width * creatureScale)) / 2;  val imgxCenter = imgx + (cbmp.width / 2)
    val imgy = (h - (cbmp.height * creatureScale)) / 8; val imgyCenter = imgy + (cbmp.height/ 2)
	image(cbmp) {
		position(imgx, imgy)
		scale = creatureScale
		smoothing = false
	}

	val texty = imgyCenter + cbmp.height / 2 + (font.fontSize * 2)

	text(creature.name, font = font, textSize = font.fontSize.toDouble(), color = creature.team.color) {
		position((w - textBounds.width) / 2, texty)
	}

	val hearty = texty + font.fontSize * 2
	val leftMargin = font.fontSize

    val hpText = text (creature.hps.toString(), font = font, textSize = font.fontSize.toDouble(), color = Colors.BLACK) {
		position(leftMargin, hearty)
	}
	image(tiles[Tile.HEART] ?: error("heart tile not found")) {
		position(leftMargin + hpText.textBounds.width, hearty)
		scale = creatureScale - 1
		smoothing = false
	}
}