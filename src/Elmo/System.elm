module Elmo.System exposing (init)

import Elmo.Types exposing (System)
import Elmo.Cpu as Cpu
import Elmo.Cpu.Ram as Ram
import Elmo.Ppu as Ppu
import Elmo.Memory as Memory


init : System
init =
    let
        ram =
            Memory.init 2048
    in
        { cpu = Cpu.init (ram |> Ram.read16 0xFFFE)
        , ppu = Ppu.init
        , ram = ram
        }
