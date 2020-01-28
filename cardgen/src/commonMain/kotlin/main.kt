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
import com.hgames.pcw.twod.Zone.Companion.solidZone
import com.hgames.pcw.twod.solidPointInt
import com.hgames.pcw.twod.toLine
import com.hgames.pcw.twod.toZone
import com.soywiz.korma.geom.PointInt

suspend fun main() = Korge(width = (24 * 9), height = ((24 * 4) + 12) * 3, bgcolor = Colors["#2b2b2b"]) {
	// val zz = PointInt(0, 0)
	// solidPointInt(zz, Colors.WHITE)
	// solidZone(zz.toLine(height.toInt(), false).growRectangleInt(-1).toZone(), Colors.GREEN)

	// return@Korge

	val dataJson = resourcesVfs["data.json"]
	val tiles = Tile.loadFromDisk(dataJson, resourcesVfs["16x16.png"].readBitmap())
	val creatures = Creature.loadFromDisk(dataJson, resourcesVfs["24x24.png"].readBitmap())
	val neutrals = Neutral.loadFromDisk(dataJson, tiles)
	val skills = Skill.loadFromDisk(dataJson)

	val font = resourcesVfs["romulus_medium_24.fnt"].readBitmapFont()

	var gendir = "/home/churlin/PERSONNEL/kcg/assets-gen"
    if (!gendir.uniVfs.exists()) gendir = "/tmp"

	val cards: MutableList<ICard> = mutableListOf()
	cards.addAll(neutrals.values)
    cards.addAll(creatures.map{ (c, bmp) -> CreatureCard(c, bmp) })

	for (card in cards) {
		val cdi = CardDrawingInput(card, font, tiles, skills)
		prepareCard(cdi)

		val bmp = renderToBitmap(this.views)
		val path = "${gendir}/${card.title}.png"
		bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
	}
}

/**
 * @param cbmp The bitmap of [creature]
 */
fun Stage.prepareCard(cdi: CardDrawingInput) {
	stage.putBackground()
	stage.putBorder(cdi)
	stage.putBorderDecoration(cdi)
    val tiley: Double = stage.putCreatureTile(cdi)
	val texty = stage.putTitle(cdi, tiley)
	when (cdi.card) {
		is CreatureCard -> stage.putStats(cdi, cdi.card, texty)
	}
}