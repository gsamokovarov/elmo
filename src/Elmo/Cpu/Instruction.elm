module Elmo.Cpu.Instruction exposing (..)

import Elmo.Cpu.Opcode as Opcode exposing (AddressingMode(..), Opcode, Label(..))
import Elmo.Types exposing (System, Cpu)
import Elmo.Memory as Memory exposing (Memory)
import Elmo.Utils exposing ((&&&), pageCrossed)


type alias Instruction =
    RuntimeAugmentation Opcode


dispatch : System -> Instruction
dispatch { cpu, memory } =
    let
        opcode =
            memory
                |> Memory.read cpu.pc
                |> Opcode.dispatch
    in
        case opcode.mode of
            Implied ->
                opcode
                    |> augment
                        { address = 0
                        , pageCrossed = False
                        }

            Accumulator ->
                opcode
                    |> augment
                        { address = 0
                        , pageCrossed = False
                        }

            Immediate ->
                opcode
                    |> augment
                        { address = cpu.pc + 1
                        , pageCrossed = False
                        }

            ZeroPage ->
                opcode
                    |> augment
                        { address = memory |> Memory.read (cpu.pc + 1)
                        , pageCrossed = False
                        }

            ZeroPageX ->
                opcode
                    |> augment
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.x)
                        , pageCrossed = False
                        }

            ZeroPageY ->
                opcode
                    |> augment
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.y)
                        , pageCrossed = False
                        }

            Absolute ->
                opcode
                    |> augment
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.x)
                        , pageCrossed = False
                        }

            AbsoluteX ->
                let
                    address =
                        memory |> Memory.read16 ((cpu.pc + 1) + cpu.x)
                in
                    opcode
                        |> augment
                            { address = address
                            , pageCrossed = pageCrossed (address - cpu.x) address
                            }

            AbsoluteY ->
                let
                    address =
                        memory |> Memory.read16 ((cpu.pc + 1) + cpu.y)
                in
                    opcode
                        |> augment
                            { address = address
                            , pageCrossed = pageCrossed (address - cpu.y) address
                            }

            Relative ->
                let
                    offset =
                        memory |> Memory.read (cpu.pc + 1)

                    address =
                        if offset < 0x80 then
                            cpu.pc + 2 + offset
                        else
                            cpu.pc + 2 + offset - 0x0100
                in
                    opcode
                        |> augment
                            { address = address
                            , pageCrossed = False
                            }

            Indirect ->
                let
                    address =
                        memory |> Memory.read16 (cpu.pc + 1)
                in
                    opcode
                        |> augment
                            { address = memory |> Memory.read16i address
                            , pageCrossed = False
                            }

            IndirectX ->
                let
                    address =
                        memory |> Memory.read (0xFF &&& (cpu.pc + 1 + cpu.x))
                in
                    opcode
                        |> augment
                            { address = memory |> Memory.read16i address
                            , pageCrossed = False
                            }

            IndirectY ->
                let
                    address =
                        memory |> Memory.read ((cpu.pc + 1) + cpu.y)
                in
                    opcode
                        |> augment
                            { address = memory |> Memory.read16i address
                            , pageCrossed = pageCrossed (address - cpu.y) address
                            }



-- INTERNAL


type alias RuntimeAugmentation opcode =
    { opcode
        | address : Int
        , pageCrossed : Bool
    }


augment : { address : Int, pageCrossed : Bool } -> Opcode -> Instruction
augment { address, pageCrossed } opcode =
    { label = opcode.label
    , mode = opcode.mode
    , opcode = opcode.opcode
    , cycles = opcode.cycles
    , pageCycles = opcode.pageCycles
    , branchPageCycles = opcode.branchPageCycles
    , bytes = opcode.bytes
    , address = address
    , pageCrossed = pageCrossed
    }
