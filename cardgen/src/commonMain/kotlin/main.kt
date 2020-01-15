import card.*
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

	val font = resourcesVfs["romulus_medium_24.fnt"].readBitmapFont()

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
	val cdi = CardDrawingInput(creature, cbmp, font, tiles)

	stage.putBackground(cdi)
	stage.putBorder(cdi)
    val tiley: Double = stage.putCreatureTile(cdi)
	val texty = stage.putCreatureName(cdi, tiley)
	stage.putStats(cdi, texty)
}