module Elmo.Core exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Memory as Memory exposing (Memory)
import Elmo.Opcode as Opcode exposing (AddressingMode, Instruction, InstructionLabel(..))


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


type alias System =
    { cpu : Cpu
    , memory : Memory
    }


step : System -> System
step system =
    let
        { cpu, memory } =
            system
    in
        if cpu.stall > 0 then
            { system | cpu = { cpu | stall = cpu.stall - 1 } }
        else
            memory
                |> Memory.read cpu.pc
                |> Opcode.dispatch
                |> process system


process : System -> Instruction -> System
process system instruction =
    case instruction.label of
        NOP ->
            nop system

        _ ->
            system



-- INSTRUCTION HANDLERS


nop : System -> System
nop system =
    system



-- STACK


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPush : System -> Int -> System
stackPush system value =
    let
        { cpu, memory } =
            system
    in
        -- TODO(genadi): The stack is between 0x0100-0x01FF, make sure to keep it
        -- around that length and handle overflow and underflows correctly.
        { system
            | cpu = { cpu | sp = cpu.sp - 1 }
            , memory = memory |> Memory.write cpu.sp value
        }


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPull : System -> ( System, Int )
stackPull system =
    let
        { cpu, memory } =
            system
    in
        ( { system | cpu = { cpu | sp = cpu.sp + 1 } }
        , Memory.read (cpu.sp + 1) memory
        )
