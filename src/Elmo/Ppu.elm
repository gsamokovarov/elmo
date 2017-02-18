module Elmo.Ppu exposing (..)

import Elmo.Types exposing (Ppu)
import Array


init : Ppu
init =
    { ppuctrl = 0
    , ppumask = 0
    , ppustatus = 0
    , oamaddr = 0
    , oamdata = 0
    , ppuscroll = 0
    , ppuaddr = 0
    , ppudata = 0
    , oamdma = 0
    , ppudataBuffer = 0
    , v = 0
    , t = 0
    , x = 0
    , w = False
    , cycle = 0
    , frame = 0
    , scanline = 0
    , vram = Array.repeat 0 0x0800
    , spriteRam = Array.repeat 0 0x0100
    }
