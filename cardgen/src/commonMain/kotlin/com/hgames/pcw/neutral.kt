package com.hgames.pcw

import com.hgames.pcw.KcgColors.NEUTRAL_COLORS
import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json
import com.hgames.pcw.twod.Tile
import com.hgames.pcw.twod.findTile

enum class Neutral {
    GREEN_POTION,
    RED_POTION;

    companion object {

        /**
         * @param file The file data.json
         * @param tiles the tiles in data.json
         */
        suspend fun <T : Bitmap> loadFromDisk(file: VfsFile, tiles: Map<Tile, BitmapSlice<T>>) : Map<Neutral, ICard> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyMap()
            val topList: List<*>? = topLevel["neutral"] as? List<*>
            val data: List<Pair<Neutral, ICard>> = topList?.map { x -> readNeutral(tiles, x) } ?: emptyList()
            println("Read ${data.size} neutral entries from disk")
            return data.toMap()
        }

        private fun <T : Bitmap> readNeutral(tiles: Map<Tile, BitmapSlice<T>>, input: Any?) : Pair<Neutral, ICard> {
            val map: Map<*, *> = input as? Map<*, *> ?: throw IllegalStateException()

            val name: String = map["name"] as? String ?: throw IllegalStateException("Neutral misses field \"name\"")
            val genErrMsg: (String) -> String = { x: String -> "Neutral $name misses field \"${x}\"" }
            val tile: String = map["tile"] as? String ?: genErrMsg("tile")
            val tileEnum: Tile = findTile(tile) ?: throw IllegalStateException("Tile named \"$tile\" not found")
            val bmp: BitmapSlice<T> = tiles[tileEnum] as? BitmapSlice<T> ?: throw IllegalStateException("No bitmap for tile \"$tile\"")
            val neutral: Neutral = findNeutral(tile) ?: throw IllegalStateException("Neutral named \"$tile\" not found")

            val card: ICard = object : ICard {
                override val title: String get() = name
                override fun getBitmap(): BitmapSlice<Bitmap> { return bmp }
                override fun getColorTheme(): ColorTheme { return NEUTRAL_COLORS }
            }

            return Pair(neutral, card)
        }
    }
}

private fun findNeutral(s: String?): Neutral? {
    if (s == null) return null
    return Neutral.values().firstOrNull { it.name.equals(s, true) }
}
