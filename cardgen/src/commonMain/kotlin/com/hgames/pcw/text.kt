package com.hgames.pcw

import com.soywiz.korge.view.Stage
import com.soywiz.korge.view.position
import com.soywiz.korge.view.text
import com.soywiz.korim.color.RGBA
import com.soywiz.korim.font.BitmapFont

fun Stage.putJustifiedText(text: String, font: BitmapFont, color: RGBA, x: Double, y: Double, width: Double) {
    val split = text.split(" ")
    var yoffset = 0
    var consumedx = 0
    val spaceWidth = font.measureWidth(" ")
    for (word in split) {
        val wordWidth = font.measureWidth(word)
        if (consumedx + wordWidth > width) {
            /* Insert a new line */
            yoffset += font.lineHeight
            consumedx = 0
        }
        text(word, font.fontSize.toDouble(), color, font) {
            position(x + consumedx, y + yoffset)
        }
        consumedx += wordWidth + spaceWidth
    }
}

data class TextBucket(val text: String, val font: BitmapFont, val color: RGBA)

fun Stage.putJustifiedText2(bucket1: TextBucket, bucket2: TextBucket, x: Double, y: Double, width: Double) {
    var yoffset = 0
    var consumedx = 0
    var spaceWidth = bucket1.font.measureWidth(" ")
    for (word in bucket1.text.split(" ")) {
        val wordWidth = bucket1.font.measureWidth(word)
        if (consumedx + wordWidth > width) {
            /* Insert a new line */
            yoffset += bucket1.font.lineHeight
            consumedx = 0
        }
        text(word, bucket1.font.fontSize.toDouble(), bucket1.color, bucket1.font) {
            position(x + consumedx, y + yoffset)
        }
        consumedx += wordWidth + spaceWidth
    }
    if (bucket2.font.fontSize < bucket1.font.fontSize) yoffset += bucket1.font.fontSize - bucket2.font.fontSize // align
    spaceWidth = bucket2.font.measureWidth(" ")
    for (word in bucket2.text.split(" ")) {
        val wordWidth = bucket2.font.measureWidth(word)
        if (consumedx + wordWidth > width) {
            /* Insert a new line */
            yoffset += bucket2.font.lineHeight
            consumedx = 0
        }
        text(word, bucket2.font.fontSize.toDouble(), bucket2.color, bucket2.font) {
            position(x + consumedx, y + yoffset)
        }
        consumedx += wordWidth + spaceWidth
    }
}
