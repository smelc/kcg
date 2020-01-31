package com.hgames.pcw

import com.hgames.pcw.KcgColors.NEUTRAL_COLORS
import com.hgames.pcw.twod.Tile
import com.hgames.pcw.twod.findTile
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json

enum class Neutral {
    GREEN_POTION,
    RED_POTION;

    companion object {

        /**
         * @param file The file data.json
         * @param tiles the tiles in data.json
         */
        suspend fun <T : Bitmap> loadFromDisk(file: VfsFile, tiles: Map<Tile, BitmapSlice<T>>): Map<Neutral, ICard> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyMap()
            val topList: List<*>? = topLevel["neutral"] as? List<*>
            val data: List<Pair<Neutral, ICard>> = topList?.map { x -> readNeutral(tiles, x) } ?: emptyList()
            println("Read ${data.size} neutral entries from disk")
            return data.toMap()
        }

        private fun <T : Bitmap> readNeutral(tiles: Map<Tile, BitmapSlice<T>>, input: Any?): Pair<Neutral, ICard> {
            val map: Map<*, *> = input as Map<*, *>

            val name: String = checkNotNull(map["name"], { "Neutral misses field \"name\"" }) as String
            val genErrMsg: (String) -> String = { x: String -> "Neutral $name misses field \"${x}\"" }
            val tile: String = checkNotNull(map["tile"], { genErrMsg("tile") }) as String
            val tileEnum: Tile = checkNotNull(findTile(tile), { "Tile named \"$tile\" not found" })
            val bmp: BitmapSlice<T> = checkNotNull(tiles[tileEnum], { "No bitmap for tile \"$tile\"" })
            val neutral: Neutral = checkNotNull(findNeutral(tile), { "Neutral named \"$tile\" not found" })

            val card: ICard = object : ICard {
                override val title: String get() = name
                override fun getBitmap(): BitmapSlice<Bitmap> {
                    return bmp
                }

                override fun getColorTheme(): ColorTheme {
                    return NEUTRAL_COLORS
                }
            }

            return Pair(neutral, card)
        }
    }
}

private fun findNeutral(s: String?): Neutral? {
    if (s == null) return null
    return Neutral.values().firstOrNull { it.name.equals(s, true) }
}