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
