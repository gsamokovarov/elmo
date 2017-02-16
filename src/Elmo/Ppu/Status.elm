module Elmo.Ppu.Status
    exposing
        ( verticalBlank
        , setVerticalBlank
        , spriteHit
        , setSpriteHit
        , spriteOverflow
        , setSpriteOverflow
        )

{-| This module provide utilities for working with the PPUSTATUS register.

    The register consists of eight bits, 5 of them are unused.

    7  bit  0
    ---- ----
    VSO. ....
    |||| ||||
    |||+-++++- Least significant bits previously written into a PPU register
    |||        (due to register not being updated for this address)
    ||+------- Sprite overflow. The intent was for this flag to be set
    ||         whenever more than eight sprites appear on a scanline, but a
    ||         hardware bug causes the actual behavior to be more complicated
    ||         and generate false positives as well as false negatives; see
    ||         PPU sprite evaluation. This flag is set during sprite
    ||         evaluation and cleared at dot 1 (the second dot) of the
    ||         pre-render line.
    |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    |          a nonzero background pixel; cleared at dot 1 of the pre-render
    |          line.  Used for raster timing.
    +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
               Set at dot 1 of line 241 (the line *after* the post-render
               line); cleared after reading $2002 and at dot 1 of the
               pre-render line.

    See http://wiki.nesdev.com/w/index.php?title=PPU_registers#PPUSTATUS
-}

import Elmo.Utils exposing (setBit)


verticalBlank : Int
verticalBlank =
    0x80


setVerticalBlank : Bool -> Int -> Int
setVerticalBlank =
    setBit verticalBlank


spriteHit : Int
spriteHit =
    0x40


setSpriteHit : Bool -> Int -> Int
setSpriteHit =
    setBit spriteHit


spriteOverflow : Int
spriteOverflow =
    0x20


setSpriteOverflow : Bool -> Int -> Int
setSpriteOverflow =
    setBit spriteOverflow
