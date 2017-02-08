module Elmo.Types exposing (..)

{-| Shared types across different Elmo modules.
-}

import Elmo.Memory as Memory exposing (Memory)


-- SYSTEM


type alias System =
    { cpu : Cpu
    , memory : Memory
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
