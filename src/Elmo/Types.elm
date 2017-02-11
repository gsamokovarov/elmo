module Elmo.Types exposing (..)

{-| Shared types across different Elmo modules.
-}

import Elmo.Memory as Memory exposing (Memory)


-- SYSTEM


type alias System =
    { cpu : Cpu
    , memory : Memory
    , ppu : Ppu
    }



-- CPU


type Interrupt
    = NMI
    | IRQ


type alias Cpu =
    { pc : Int
    , sp : Int
    , a : Int
    , x : Int
    , y : Int
    , p : Int
    , interrupt : Maybe Interrupt
    , stall : Int
    , cycles : Int
    }



-- PPU


type alias Ppu =
    { ppuctrl : Int
    , ppumask : Int
    , ppustatus : Int
    , oamaddr : Int
    , oamdata : Int
    , ppuscroll : Int
    , ppuaddr : Int
    , ppudata : Int
    , oamdma : Int
    , v : Int
    , t : Int
    , x : Int
    , w : Int
    }
