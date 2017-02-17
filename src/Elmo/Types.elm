module Elmo.Types exposing (..)

{-| Shared types across different Elmo modules.

# Types
@docs System, Interrupt, Cpu, Ppu
-}

import Array exposing (Array)


-- SYSTEM


{-| Representation of the general NES system state.

The instructions should accept this as their input and output, as most them
touch up on multiple components.
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

For reference for the v, t, x, w registers, see
http://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_registers

For reference for the ppu and oam registers, see
https://wiki.nesdev.com/w/index.php?title=PPU_registers
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
    , ppudataBuffer : Int
    , v : Int
    , t : Int
    , x : Int
    , w : Bool
    , cycle : Int
    , frame : Int
    , scanline : Int
    , memory : Memory
    , spriteMemory : Memory
    }



-- MEMORY


{-| Representation of linear memory. Needed by the CPU and the PPU.
-}
type alias Memory =
    Array Int
