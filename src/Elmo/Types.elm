module Elmo.Types exposing (..)

{-| Shared types across different Elmo modules.

# Types
@docs System, Interrupt, Cpu, Ppu
-}

import Elmo.Memory as Memory exposing (Memory)


-- SYSTEM


{-| Representation of the general NES system state.

The instructions should accept this as their input and output, as most the instructions
-}
type alias System =
    { cpu : Cpu
    , memory : Memory
    , ppu : Ppu
    }



-- CPU


{-| CPU interrupt.

# IRQ

An interrupt request.

See https://wiki.nesdev.com/w/index.php/IRQ

# NMI

Non-maskable interrupt. Connected to the PPU and used to detect vertical
blanking.

See https://wiki.nesdev.com/w/index.php/NMI
-}
type Interrupt
    = NMI
    | IRQ


{-| Representation of the CPU state at a given time.
-}
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


{-| Representation of the PPU state at a given time.
-}
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
    , cycle : Int
    , frame : Int
    , scanLine : Int
    }
