module Elmo.Ppu.VRam exposing (..)

{-| The PPU addresses a 16kB space, $0000-3FFF, completely separate
from the CPU's address bus. It is either directly accessed by the PPU
itself, or via the CPU with memory mapped registers at $2006 and
$2007.

The NES has 2kB of RAM dedicated to the PPU, normally mapped to the
nametable address space from $2000-2FFF, but this can be rerouted
through custom cartridge wiring.

Address range   Size    Description
$0000-$0FFF     $1000   Pattern table 0
$1000-$1FFF     $1000   Pattern Table 1
$2000-$23FF     $0400   Nametable 0
$2400-$27FF     $0400   Nametable 1
$2800-$2BFF     $0400   Nametable 2
$2C00-$2FFF     $0400   Nametable 3
$3000-$3EFF     $0F00   Mirrors of $2000-$2EFF
$3F00-$3F1F     $0020   Palette RAM indexes
$3F20-$3FFF     $00E0   Mirrors of $3F00-$3F1F
-}

import Elmo.Types exposing (System)
import Elmo.Memory as Memory


read : Int -> System -> Int
read address ({ ppu } as system) =
    if address < 0x2000 then
        0
    else if address < 0x3F00 then
        ppu.vRam |> Memory.get (address % 0x0800)
    else if address < 0x4000 then
        ppu.paletteRam |> Memory.get (address % 0x20)
    else
        0
