module Elmo.Cpu.Stack exposing (..)

{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}

import Elmo.Memory as Memory
import Elmo.Types exposing (System)
import Elmo.Utils exposing ((&&&), (|||), (<<<), (>>>))


push : Int -> System -> System
push value ({ cpu, memory } as system) =
    { system
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp - 1) }
        , memory = memory |> Memory.write cpu.sp value
    }


push16 : Int -> System -> System
push16 value ({ cpu, memory } as system) =
    system
        |> push ((cpu.pc >>> 8) &&& 0xFF)
        |> push (cpu.pc &&& 0xFF)


pull : System -> ( System, Int )
pull ({ cpu, memory } as system) =
    ( { system
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp + 1) }
      }
    , Memory.read (cpu.sp + 1) memory
    )


pull16 : System -> ( System, Int )
pull16 ({ cpu, memory } as system) =
    let
        ( systemAfterHigh, high ) =
            system |> pull

        ( systemAfterLow, low ) =
            systemAfterHigh |> pull
    in
        ( systemAfterLow, (high <<< 8) ||| low )
