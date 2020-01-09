import com.soywiz.korge.*
import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.*
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.format.*
import com.soywiz.korio.file.std.*
import com.soywiz.korma.geom.RectangleInt
import creatures.Creature
import twod.shrink
import twod.solidInnerBorders

/**
 * 96/24 = 4 and 96/16 = 6
 * 144/24 = 6 and 144/16 = 9
 */
suspend fun main() = Korge(width = 24 * 3, height = (24 * 4) + 12, bgcolor = Colors["#2b2b2b"]) {
	val creaturesBmp : Bitmap = resourcesVfs["creatures.png"].readBitmap()
	val items = resourcesVfs["oryx_16bit_fantasy_items_trans.png"].readBitmap()
	val creatures = Creature.loadFromDisk(resourcesVfs["creatures.json"], creaturesBmp)

	for ((i, p) in creatures.withIndex()) {
		prepareCard(p.first, p.second)

		val bmp = renderToBitmap(this.views)
		val path = "/tmp/demo${i}.png"
		bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
	}
}

/**
 * @param cbmp The bitmap of [creature]
 */
fun Stage.prepareCard(creature: Creature, cbmp: BitmapSlice<Bitmap>) {
	val h : Double = height
    val w = width

	val background: RGBA = RGBA.unclamped(247, 232, 150, 255)
	solidRect(w, h, background)
    val rect: RectangleInt = RectangleInt.invoke(0, 0, w.toInt(), h.toInt())
	solidInnerBorders(rect.shrink(), Colors.BLACK)

	image(cbmp) {
		position((w - cbmp.width) / 2, (h - cbmp.height) / 3)
		smoothing = false
	}
}