import com.hgames.pcw.*
import com.soywiz.korge.Korge
import com.soywiz.korge.view.Stage
import com.soywiz.korge.view.renderToBitmap
import com.soywiz.korim.color.Colors
import com.soywiz.korim.font.readBitmapFont
import com.soywiz.korim.format.PNG
import com.soywiz.korim.format.readBitmap
import com.soywiz.korim.format.writeTo
import com.soywiz.korio.file.std.resourcesVfs
import com.soywiz.korio.file.std.uniVfs
import com.hgames.pcw.twod.Tile
import com.soywiz.korev.Key
import com.soywiz.korge.input.keys
import kotlin.math.max
import kotlin.math.min

suspend fun main() = Korge(width = (24 * 9), height = ((24 * 4) + 12) * 3, bgcolor = Colors["#2b2b2b"]) {
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
    cards.addAll(creatures.map { (c, bmp) -> CreatureCard(c, bmp) })

    val cdis: List<CardDrawingInput> = cards.map { CardDrawingInput(it, font, tiles, skills) }
    cdis.forEach { drawCard(it, gendir) }

    var currentCard = cdis.size - 1

    keys {
        onKeyDown {
            val save = currentCard
            when (it.key) {
                Key.LEFT -> currentCard = max(0, currentCard - 1)
                Key.RIGHT -> currentCard = min(cdis.size - 1, currentCard + 1)
            }
            if (save != currentCard)
                drawCard(cdis[currentCard], null)
        }
    }
}

/**
 * @param gendir Where to render [cdi], or null not to render it
 */
suspend fun Stage.drawCard(cdi: CardDrawingInput, gendir: String?) {
    drawCard(cdi)
    val bmp = renderToBitmap(this.views)
    if (gendir != null) {
        val path = "${gendir}/${cdi.card.title}.png"
        bmp.writeTo(path.uniVfs, PNG)
        println("Written $path")
    }
}

fun Stage.drawCard(cdi: CardDrawingInput) {
    stage.putBackground()
    stage.putBorder(cdi)
    stage.putBorderDecoration(cdi)
    val tiley: Double = stage.putCreatureTile(cdi)
    val texty = stage.putTitle(cdi, tiley)
    when (cdi.card) {
        is CreatureCard -> stage.putStats(cdi, cdi.card, texty)
    }
}