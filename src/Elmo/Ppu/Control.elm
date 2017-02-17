module Elmo.Ppu.Control
    exposing
        ( nametable
        , setNametable
        , increment
        , setIncrement
        , spriteTable
        , setSpriteTable
        , backgroundTable
        , setBackgroundTable
        , spriteSize
        , setSpriteSize
        , slave
        , setSlave
        , nmi
        , setNmi
        )

{-| This module provide utilities for working with the PPUCTRL register.

    7  bit  0
    ---- ----
    VPHB SINN
    |||| ||||
    |||| ||++- Base nametable address
    |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    |||| |     (0: add 1, going across; 1: add 32, going down)
    |||| +---- Sprite pattern table address for 8x8 sprites
    ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    |||+------ Background pattern table address (0: $0000; 1: $1000)
    ||+------- Sprite size (0: 8x8; 1: 8x16)
    |+-------- PPU master/slave select
    |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    +--------- Generate an NMI at the start of the
               vertical blanking interval (0: off; 1: on)

    See http://wiki.nesdev.com/w/index.php?title=PPU_registers#PPUCTRL
-}

import Elmo.Utils exposing ((|||), (&&&), (<<<), setBit)


-- NAMETABLE


nametable : Int
nametable =
    0x03


setNametable : Int -> Int -> Int
setNametable value ctrl =
    (value &&& nametable) ||| (ctrl && 0xFFFC)



-- INCREMENT


increment : Int
increment =
    0x04


setIncrement : Bool -> Int -> Int
setIncrement value =
    setBit increment value



-- SPRITE PATTERN TABLE


spriteTable : Int
spriteTable =
    0x08


setSpriteTable : Bool -> Int -> Int
setSpriteTable value =
    setBit spriteTable value



-- BACKGROUND PATTERN TABLE


backgroundTable : Int
backgroundTable =
    0x10


setBackgroundTable : Bool -> Int -> Int
setBackgroundTable value =
    setBit backgroundTable value



-- SPRITE SIZE


spriteSize : Int
spriteSize =
    0x20


setSpriteSize : Bool -> Int -> Int
setSpriteSize value =
    setBit spriteSize value



-- SLAVE


slave : Int
slave =
    0x40


setSlave : Bool -> Int -> Int
setSlave value =
    setBit slave value



-- NMI


nmi : Int
nmi =
    0x80


setNmi : Bool -> Int -> Int
setNmi value =
    setBit nmi value
