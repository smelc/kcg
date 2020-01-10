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
import twod.shrink
import twod.solidInnerBorders

suspend fun main() = Korge(width = (24 * 9), height = ((24 * 4) + 12) * 3, bgcolor = Colors["#2b2b2b"]) {
	val items = resourcesVfs["oryx_16bit_fantasy_items_trans.png"].readBitmap()
	val creatures = Creature.loadFromDisk(resourcesVfs["creatures.json"], resourcesVfs["creatures.png"].readBitmap())

	val font = resourcesVfs["romulus.fnt"].readBitmapFont()

	for ((i, p) in creatures.withIndex()) {
		prepareCard(p.first, p.second, font)

		val bmp = renderToBitmap(this.views)
		val path = "/tmp/demo${i}.png"
		bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
	}
}

/**
 * @param cbmp The bitmap of [creature]
 */
fun Stage.prepareCard(creature: Creature, cbmp: BitmapSlice<Bitmap>, font: BitmapFont) {
	val h : Double = height
    val w = width

	val background: RGBA = RGBA.unclamped(247, 232, 150, 255)
	solidRect(w, h, background)
    val rect: RectangleInt = RectangleInt.invoke(0, 0, w.toInt(), h.toInt())
	solidInnerBorders(rect.shrink(), creature.team.color)

	val creatureScale = 3.0
	val imgx = (w - (cbmp.width * creatureScale)) / 2;  val imgxCenter = imgx + (cbmp.width / 2)
    val imgy = (h - (cbmp.height * creatureScale)) / 6; val imgyCenter = imgy + (cbmp.height/ 2)
	image(cbmp) {
		position(imgx, imgy)
		scale = creatureScale
		smoothing = false
	}

	text(creature.name, font = font, textSize = font.fontSize.toDouble(), color = creature.team.color) {
		position((w - textBounds.width) / 2, imgyCenter + cbmp.height / 2 + (font.fontSize * 2))
	}
}