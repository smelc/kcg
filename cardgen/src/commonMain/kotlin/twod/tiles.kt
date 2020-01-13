package twod

import com.soywiz.korim.bitmap.Bitmap
import com.soywiz.korim.bitmap.BitmapSlice
import com.soywiz.korim.bitmap.sliceWithSize
import com.soywiz.korio.file.VfsFile
import com.soywiz.korio.serialization.json.Json
import com.soywiz.korma.geom.RectangleInt

enum class Tile {
    HEART,
    SWORD;

    companion object {

        /**
         * @param file The file data.json
         * @param bmp The bitmap of tiles.png
         */
        suspend fun loadFromDisk(file: VfsFile, bmp: Bitmap) : Map<Tile, BitmapSlice<Bitmap>> {
            val topLevel: Map<*, *> = Json.parse(file.readString()) as? Map<*, *> ?: return emptyMap()
            val topList: List<*>? = topLevel["tiles"] as? List<*>
            val data: List<Pair<Tile, RectangleInt>> = topList?.map { x -> readTile(x)} ?: emptyList()
            println("Read ${data.size} tiles from disk")
            return data.map { (t, r) -> Pair(t, bmp.sliceWithSize(r.x, r.y, r.width, r.height)) }.associate { p -> p }
        }

        private fun readTile(input: Any?) : Pair<Tile, RectangleInt> {
            val map: Map<*, *> = input as? Map<*, *> ?: throw IllegalStateException()

            val name: String = map["name"] as? String ?: throw IllegalStateException("Tile misses field \"name\"")
            val tile: Tile = findTile(name) ?: throw IllegalStateException("Tile named \"$name\" not found")
            val genErrMsg: (String) -> String = { x: String -> "Tile $name misses field \"${x}\"" }

            val x: Int = map["x"] as? Int ?: throw IllegalStateException(genErrMsg("x"))
            val y: Int = map["y"] as? Int ?: throw IllegalStateException(genErrMsg("y"))
            val w: Int = map["w"] as? Int ?: 16 // default
            val h: Int = map["h"] as? Int ?: 16 // default
            val rect = RectangleInt(x, y, w, h)

            return Pair(tile, rect)
        }
    }
}

private fun findTile(s: String?): Tile? {
    if (s == null) return null
    return Tile.values().firstOrNull { it.name.equals(s, true) }
}
