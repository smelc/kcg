import com.hgames.pcw.*
import com.soywiz.korge.Korge
import com.soywiz.korge.view.Stage
import com.soywiz.korge.view.renderToBitmap
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.color.Colors
import com.soywiz.korim.font.BitmapFont
import com.soywiz.korim.font.readBitmapFont
import com.soywiz.korim.format.PNG
import com.soywiz.korim.format.readBitmap
import com.soywiz.korim.format.writeTo
import com.soywiz.korio.file.std.resourcesVfs
import com.soywiz.korio.file.std.uniVfs
import com.hgames.pcw.twod.Tile

suspend fun main() = Korge(width = (24 * 9), height = ((24 * 4) + 12) * 3, bgcolor = Colors["#2b2b2b"]) {
	val dataJson = resourcesVfs["data.json"]
	val tiles = Tile.loadFromDisk(dataJson, resourcesVfs["16x16.png"].readBitmap())
	val creatures = Creature.loadFromDisk(dataJson, resourcesVfs["24x24.png"].readBitmap())
	val neutrals = Neutral.loadFromDisk(dataJson, tiles)

	val font = resourcesVfs["romulus_medium_24.fnt"].readBitmapFont()

	var gendir = "/home/churlin/PERSONNEL/kcg/assets-gen"
    if (!gendir.uniVfs.exists()) gendir = "/tmp"

	val cards: MutableList<ICard> = mutableListOf()
    cards.addAll(creatures.map{ (c, bmp) -> CreatureCard(c, bmp) })
	cards.addAll(neutrals.values)

	for (card in cards) {
		prepareCard(card, font, tiles)

		val bmp = renderToBitmap(this.views)
		val path = "${gendir}/${card.title}.png"
		bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
	}
}

/**
 * @param cbmp The bitmap of [creature]
 */
fun Stage.prepareCard(card: ICard, font: BitmapFont, tiles: Map<Tile, BitmapSlice<Bitmap>>) {
	val cdi = CardDrawingInput(card, font, tiles)

	stage.putBackground()
	stage.putBorder(cdi)
	stage.putBorderDecoration(cdi)
    val tiley: Double = stage.putCreatureTile(cdi)
	val texty = stage.putTitle(cdi, tiley)
	when (card) {
		is CreatureCard -> stage.putStats(cdi, card, texty)
	}
}