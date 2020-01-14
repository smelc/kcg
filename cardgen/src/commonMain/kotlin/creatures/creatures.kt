package creatures

import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.bitmap.sliceWithSize
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json
import com.soywiz.korma.geom.RectangleInt

enum class Team(val color: RGBA) {
    HUMAN(RGBA.unclamped(14, 123, 178, 255)),
    ORC(Colors.RED);
}

private fun findTeam(s: String?): Team? {
    if (s == null) return null
    return Team.values().firstOrNull { it.name.equals(s, true) }
}

enum class Skill {
    HitFromBack
}

data class Creature(val name: String, val team: Team, var hps: Int, val attack: Int, val victoryPoints: Int, val skill: List<Skill>) {

    companion object {

        /**
         * @param creaturesBmp The bitmap of creatures.png
         */
        suspend fun loadFromDisk(file: VfsFile, creaturesBmp: Bitmap) : List<Pair<Creature, BitmapSlice<Bitmap>>> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyList()
            val topList: List<*>? = topLevel["creatures"] as? List<*>
            val data = topList?.map { x -> readCreature(x)} ?: emptyList();
            println("Read ${data.size} creatures from disk")
            return data.map { (c, r) -> Pair(c, creaturesBmp.sliceWithSize(r.x, r.y, r.width, r.height)) }
        }

        private fun readCreature(input: Any?) : Pair<Creature, RectangleInt> {
            val map: Map<*, *> = input as? Map<*, *> ?: throw IllegalStateException()

            val name: String = map["name"] as? String ?: throw IllegalStateException("Creature misses field \"name\"")
            val genErrMsg: (String) -> String = { x: String -> "Creature $name misses field \"${x}\"" }

            val teamString: String = map["team"] as? String ?: throw IllegalStateException(genErrMsg("team"))
            val team: Team = findTeam(teamString) ?: throw IllegalStateException("No such team: $teamString")
            val hp: Int = map["hp"] as? Int ?: throw IllegalStateException(genErrMsg("hp"))
            val attack: Int = map["attack"] as? Int ?: throw IllegalStateException(genErrMsg("attack"))
            val victoryPoints: Int = map["victory_points"] as? Int ?: throw IllegalStateException(genErrMsg("victoryPoints"))
            val skills: List<Skill> = readSkill(map["skills"])

            val x: Int = map["x"] as? Int ?: throw IllegalStateException(genErrMsg("x"))
            val y: Int = map["y"] as? Int ?: throw IllegalStateException(genErrMsg("y"))
            val w: Int = map["w"] as? Int ?: 24 // default
            val h: Int = map["h"] as? Int ?: 24 // default
            val rect = RectangleInt(x, y, w, h)

            return Pair(Creature(name, team, hp, attack, victoryPoints, skills), rect)
        }

        private fun readSkill(input: Any?) : List<Skill> {
            return emptyList()
        }

    }

}
