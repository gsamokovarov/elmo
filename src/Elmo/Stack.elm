module Elmo.Stack exposing (..)

import Elmo.Memory as Memory exposing (Memory)
import Elmo.Types exposing (System)
import Elmo.Utils exposing ((&&&))


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
push : System -> Int -> System
push ({ cpu, memory } as system) value =
    -- TODO(genadi): The stack is between 0x0100-0x01FF, make sure to keep it
    -- around that length and handle overflow and underflows correctly.
    { system
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp - 1) }
        , memory = memory |> Memory.write cpu.sp value
    }


pull : System -> ( System, Int )
pull ({ cpu, memory } as system) =
    ( { system
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp + 1) }
      }
    , Memory.read (cpu.sp + 1) memory
    )
