module Elmo.System exposing (init)

import Elmo.Types exposing (System)
import Elmo.Cpu as Cpu
import Elmo.Ppu as Ppu
import Elmo.Memory as Memory


init : System
init =
    let
        memory =
            Memory.init
    in
        { cpu = Cpu.init (memory |> Memory.read16 0xFFFE)
        , ppu = Ppu.init
        , memory = memory
        }
