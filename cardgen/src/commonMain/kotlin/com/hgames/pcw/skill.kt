package com.hgames.pcw

import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json

enum class Skill {
    HitFromBack,
    Flammable,
    Leader,
    Ranged,
    Unique;

    companion object {

        /**
         * @param file The file data.json
         * @param tiles the skills in data.json, there's at most one entry per member of [Skill]
         */
        suspend fun loadFromDisk(file: VfsFile): List<SkillData> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyList()
            val topList: List<*>? = topLevel["skills"] as? List<*>
            val data: List<SkillData> = topList?.map(::readSkill) ?: emptyList()
            println("""Read ${data.size} skill entries from disk ${data.joinToString(" ") { it.title }}""")
            return data
        }

        private fun readSkill(input: Any?): SkillData {
            val map: Map<*, *> = input as? Map<*, *> ?: throw IllegalStateException()

            val name: String = map["name"] as? String ?: throw IllegalStateException("Skill misses field \"name\"")
            val genErrMsg: (String) -> String = { x: String -> "Skill $name misses field \"${x}\"" }
            val skill: Skill = checkNotNull(findSkill(name), { "Skill not found: \"$name\"" })
            val title: String = checkNotNull(map["title"], { genErrMsg("title") }) as String
            val text: String = checkNotNull(map["text"], { genErrMsg("text") }) as String

            return SkillData(skill, title, text)
        }
    }
}

data class SkillData(val skill: Skill, val title: String, val text: String)

fun findSkill(s: String?): Skill? {
    if (s == null) return null
    return Skill.values().firstOrNull { it.name.equals(s, true) }
}
