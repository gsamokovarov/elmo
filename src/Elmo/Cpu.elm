module Elmo.Cpu exposing (..)

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


step : Cpu -> Memory -> ( Cpu, Memory )
step cpu memory =
    if cpu.stall > 0 then
        ( { cpu | stall = cpu.stall - 1 }
        , memory
        )
    else
        memory
            |> Memory.read cpu.pc
            |> Opcode.dispatch
            |> process cpu memory


process : Cpu -> Memory -> Instruction -> ( Cpu, Memory )
process cpu memory instruction =
    case instruction.label of
        NOP ->
            nop cpu memory

        _ ->
            ( cpu, memory )



-- INSTRUCTION HANDLERS


nop : Cpu -> Memory -> ( Cpu, Memory )
nop cpu memory =
    ( cpu, memory )



-- STACK


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPush cpu memory value =
    -- TODO(genadi): The stack is between 0x0100-0x01FF, make sure to keep it
    -- around that length and handle overflow and underflows correctly.
    ( { cpu | sp = cpu.sp - 1 }
    , memory |> Memory.write cpu.sp value
    )


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPull cpu memory =
    ( { cpu | sp = cpu.sp + 1 }
    , memory |> Memory.read (cpu.sp + 1)
    )
