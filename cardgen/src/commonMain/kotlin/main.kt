import com.soywiz.klock.seconds
import com.soywiz.korge.*
import com.soywiz.korge.tween.*
import com.soywiz.korge.view.*
import com.soywiz.korim.bitmap.*
import com.soywiz.korim.color.Colors
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.format.*
import com.soywiz.korio.async.launchImmediately
import com.soywiz.korio.file.std.*
import com.soywiz.korma.geom.degrees
import com.soywiz.korma.interpolation.Easing

suspend fun main() = Korge(width = 512, height = 512, bgcolor = Colors["#2b2b2b"]) {
	val creatures = resourcesVfs["oryx_16bit_fantasy_creatures_trans.png"].readBitmap()
	val blueKnight = creatures.sliceWithSize(24, 24, 24, 24);
	val goodArcher = creatures.sliceWithSize(72, 24, 24, 24);
	val goblin = creatures.sliceWithSize(24, 360, 24, 24);

	val h : Double = height
    val w = width

	val xStep = w / 4
	val yStep = h / 5

	for (x in 0 .. 2) {
        val archer = x == 1
		image(if (archer) goodArcher else blueKnight) {
			position(xStep * (x + 1), yStep * (if (archer) 1 else 2))
			scale(2f)
		}
	}

	for (x in 0 .. 2) {
		println(x)
		image(goblin) {
			position(xStep * (x + 1), yStep * 3)
			scale(2f)
		}
	}

    // val bitmapRgba : RGBA = RGBA.unclamped(255, 255, 255, 150)
    // val out = Bitmap32(128, 128, bitmapRgba, false)
	val bmp = renderToBitmap(this.views)
	bmp.writeTo("/tmp/demo.png".uniVfs, PNG)

	/* launchImmediately {
		while (true) {
			image.tween(image::rotation[minDegrees], time = 1.seconds, easing = Easing.EASE_IN_OUT)
			image.tween(image::rotation[maxDegrees], time = 1.seconds, easing = Easing.EASE_IN_OUT)
		}
	} */
}