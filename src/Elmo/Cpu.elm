module Elmo.Cpu exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Memory exposing (Memory)
import Elmo.Opcode exposing (..)


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
    , mode : AddressingMode
    , stall : Int
    , cycles : Int
    , memory : Memory
    }


step : ( Cpu, Memory ) -> ( Cpu, Memory )
step ( cpu, memory ) =
    if cpu.stall > 0 then
        ( { cpu | stall = cpu.stall - 1 }, memory )
    else
        ( cpu, memory )
