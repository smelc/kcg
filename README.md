# Korge Card Generator

This project started as a card game using the amazing
[korge](https://korge.soywiz.com/) (in Kotlin). korge was used to generate
the cards, such as this one:

<p align="center">
  <img src="https://i.imgur.com/yJeMc3E.png"/>
</p>

Then I planned doing the AI using Haskell, to benchmark the
different teams; to make sure the game's balance is correct. However,
I drifted doing the game's UI using the great
[gloss](https://hackage.haskell.org/package/gloss/docs/Graphics-Gloss.html)
for OpenGL rendering. But it soon appeared to me I wouldn't learn
a lot compared to my [previous games](https://hgames.itch.io/) done
with OpenGL too (with Java's [libgdx](https://libgdx.badlogicgames.com/)).
In particular I would lack the support for doing the GUI and structuring
the view that I had with libgdx's [scene2d](https://github.com/libgdx/libgdx/wiki/Scene2d).

As I wanted to continue doing Haskell, I switched to using
[miso](https://github.com/dmjio/miso) where I would have support for
a structured GUI, as well as having cross-compilation to Javascript which
is the number 1 platform for gaming and I would have nice
font rendering at any size without any trouble (damn you bitmap fonts!).
And in addition, I would learn new stuff.
As a consequence I stopped
development in this repo, but the same game continues with miso here:
[miso-darkcraw](https://github.com/smelc/miso-darkcraw).

For the record, here's the state of the game in this repo; it is capable
of displaying a board with cards:

<p align="center">
  <img src="https://i.imgur.com/bdj4Zhw.png"/>
</p>
