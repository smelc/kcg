package com.hgames.pcw

import com.hgames.pcw.KcgColors.HUMAN_COLORS
import com.hgames.pcw.KcgColors.ORC_COLORS
import com.hgames.pcw.KcgColors.UNDEAD_COLORS
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.bitmap.sliceWithSize
import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json
import com.soywiz.korma.geom.RectangleInt

enum class Team(val color: ColorTheme) {
    HUMAN(HUMAN_COLORS),
    ORC(ORC_COLORS),
    UNDEAD(UNDEAD_COLORS)
}

private fun findTeam(s: String?): Team? {
    if (s == null) return null
    return Team.values().firstOrNull { it.name.equals(s, true) }
}

data class Creature(val name: String, val title: String, val team: Team, var hps: Int, val attack: Int, val moral: Int?, val victoryPoints: Int, val skills: List<Skill>) {

    companion object {

        /**
         * @param creaturesBmp The bitmap of creatures.png
         */
        suspend fun loadFromDisk(file: VfsFile, creaturesBmp: Bitmap): List<Pair<Creature, BitmapSlice<Bitmap>>> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyList()
            val topList: List<*>? = topLevel["creatures"] as? List<*>
            val data = topList?.map { x -> readCreature(x) } ?: emptyList();
            println("""Read ${data.size} creatures from disk: ${data.joinToString(" ") { it.first.name }}""")
            return data.map { (c, r) -> Pair(c, creaturesBmp.sliceWithSize(r.x, r.y, r.width, r.height)) }
        }

        private fun readCreature(input: Any?): Pair<Creature, RectangleInt> {
            val map: Map<*, *> = input as? Map<*, *> ?: throw IllegalStateException()

            val id: Map<*, *> = map["id"] as? Map<*, *> ?: throw IllegalStateException("Creature misses field \"id\"")
            val readID = readID(id)
            val name = readID.first
            val teamString: String = readID.second
            val genErrMsg: (String) -> String = { x: String -> "Creature $name misses field \"${x}\"" }

            val title: String = map["title"] as? String ?: throw IllegalStateException(genErrMsg("title"))
            val team: Team = findTeam(teamString) ?: throw IllegalStateException("No such team: $teamString")
            val hp: Int = map["hp"] as? Int ?: throw IllegalStateException(genErrMsg("hp"))
            val attack: Int = map["attack"] as? Int ?: throw IllegalStateException(genErrMsg("attack"))
            val moral: Int? = map["moral"] as? Int
            val victoryPoints: Int = map["victory_points"] as? Int
                    ?: throw IllegalStateException(genErrMsg("victory_points"))
            val skills: List<Skill> = readSkill(map["skills"])

            val x: Int = map["x"] as? Int ?: throw IllegalStateException(genErrMsg("x"))
            val y: Int = map["y"] as? Int ?: throw IllegalStateException(genErrMsg("y"))
            val w: Int = map["w"] as? Int ?: 72 // default
            val h: Int = map["h"] as? Int ?: 72 // default
            val rect = RectangleInt(x, y, w, h)

            return Pair(Creature(name, title, team, hp, attack, moral, victoryPoints, skills), rect)
        }

        private fun readID(map: Map<*, *>): Pair<String, String> {
            val name: String = map["name"] as? String ?: throw IllegalStateException("\"id\" misses field \"name\"")
            val team: String = map["team"] as? String ?: throw IllegalStateException("\"id\" misses field \"team\"")
            return Pair (name, team)
        }

        private fun readSkill(input: Any?): List<Skill> {
            if (input == null) return emptyList()
            val list: List<*> = input as? List<*> ?: throw IllegalStateException("\"skills\" is not mapped to a list, found: $input")
            if (list.isNullOrEmpty()) return emptyList()
            val result: MutableList<Skill> = mutableListOf()
            for (member in list) {
                val string = member as? String ?: throw IllegalStateException("Member of \"skills\" list is not a string: $member")
                val skill: Skill = findSkill(member) ?: throw IllegalStateException("skill not found: $string")
                result.add(skill)
            }
            return result
        }

    }

}
