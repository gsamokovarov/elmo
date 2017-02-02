module Elmo.Core exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Memory as Memory exposing (Memory)
import Elmo.Opcode as Opcode exposing (AddressingMode(..), Opcode, Label(..))
import Elmo.Math exposing ((|+|), (&&&), (|||))
import Bitwise


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
    , memory : Memory
    }


type alias System =
    { cpu : Cpu
    , memory : Memory
    }


tick : System -> System
tick ({ cpu, memory } as system) =
    if cpu.stall > 0 then
        { system | cpu = { cpu | stall = cpu.stall - 1 } }
    else
        let
            instruction =
                dispatchInstruction system

            newSystem =
                { system
                    | cpu =
                        { cpu
                            | pc = cpu.pc + instruction.bytes
                            , cycles =
                                cpu.cycles
                                    + instruction.cycles
                                    + (if instruction.pageCrossed then
                                        instruction.pageCycles
                                       else
                                        0
                                      )
                        }
                }
        in
            {- We still need to handle interrupts here. -}
            instruction |> processInstruction newSystem



-- INSTRUCTION


type alias Instruction opcode =
    { opcode
        | address : Int
        , pageCrossed : Bool
    }


dispatchInstruction : System -> Instruction Opcode
dispatchInstruction { cpu, memory } =
    let
        pageCrossed : Int -> Int -> Bool
        pageCrossed a b =
            (a &&& 0xFF00) /= (b &&& 0xFF00)

        opcode =
            memory
                |> Memory.read cpu.pc
                |> Opcode.dispatch
    in
        case opcode.mode of
            Implied ->
                opcode
                    |> augmentToInstruction
                        { address = 0
                        , pageCrossed = False
                        }

            Accumulator ->
                opcode
                    |> augmentToInstruction
                        { address = 0
                        , pageCrossed = False
                        }

            Immediate ->
                opcode
                    |> augmentToInstruction
                        { address = cpu.pc + 1
                        , pageCrossed = False
                        }

            ZeroPage ->
                opcode
                    |> augmentToInstruction
                        { address = memory |> Memory.read (cpu.pc + 1)
                        , pageCrossed = False
                        }

            ZeroPageX ->
                opcode
                    |> augmentToInstruction
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.x)
                        , pageCrossed = False
                        }

            ZeroPageY ->
                opcode
                    |> augmentToInstruction
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.y)
                        , pageCrossed = False
                        }

            Absolute ->
                opcode
                    |> augmentToInstruction
                        { address = memory |> Memory.read ((cpu.pc + 1) + cpu.x)
                        , pageCrossed = False
                        }

            AbsoluteX ->
                let
                    address =
                        memory |> Memory.read16 ((cpu.pc + 1) + cpu.x)
                in
                    opcode
                        |> augmentToInstruction
                            { address = address
                            , pageCrossed = pageCrossed (address - cpu.x) address
                            }

            AbsoluteY ->
                let
                    address =
                        memory |> Memory.read16 ((cpu.pc + 1) + cpu.y)
                in
                    opcode
                        |> augmentToInstruction
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
                        |> augmentToInstruction
                            { address = address
                            , pageCrossed = False
                            }

            Indirect ->
                let
                    address =
                        memory |> Memory.read16 (cpu.pc + 1)
                in
                    opcode
                        |> augmentToInstruction
                            { address = memory |> Memory.read16i address
                            , pageCrossed = False
                            }

            IndirectX ->
                let
                    address =
                        memory |> Memory.read ((cpu.pc + 1) |+| cpu.x)
                in
                    opcode
                        |> augmentToInstruction
                            { address = memory |> Memory.read16i address
                            , pageCrossed = False
                            }

            IndirectY ->
                let
                    address =
                        memory |> Memory.read ((cpu.pc + 1) + cpu.y)
                in
                    opcode
                        |> augmentToInstruction
                            { address = memory |> Memory.read16i address
                            , pageCrossed = pageCrossed (address - cpu.y) address
                            }


processInstruction : System -> Instruction Opcode -> System
processInstruction system instruction =
    case instruction.label of
        NOP ->
            nop system

        AHX ->
            ahx system

        ALR ->
            alr system

        ARR ->
            arr system

        AXS ->
            axs system

        DCP ->
            dcp system

        ILL ->
            ill system

        ISC ->
            isc system

        LAS ->
            las system

        LAX ->
            lax system

        RLA ->
            rla system

        RRA ->
            rra system

        SAX ->
            sax system

        SHX ->
            shx system

        SHY ->
            shy system

        SLO ->
            slo system

        SRE ->
            sre system

        TAS ->
            tas system

        XAA ->
            xaa system

        _ ->
            system


augmentToInstruction : { address : Int, pageCrossed : Bool } -> Opcode -> Instruction Opcode
augmentToInstruction { address, pageCrossed } opcode =
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



-- INSTRUCTION HANDLERS


nop : System -> System
nop system =
    system



{- The following instructions are unused on the 2A03, but are still present on
   the 6502.
-}


ahx : System -> System
ahx system =
    system


alr : System -> System
alr system =
    system


arr : System -> System
arr system =
    system


axs : System -> System
axs system =
    system


dcp : System -> System
dcp system =
    system


ill : System -> System
ill system =
    system


isc : System -> System
isc system =
    system


las : System -> System
las system =
    system


lax : System -> System
lax system =
    system


rla : System -> System
rla system =
    system


rra : System -> System
rra system =
    system


sax : System -> System
sax system =
    system


shx : System -> System
shx system =
    system


shy : System -> System
shy system =
    system


slo : System -> System
slo system =
    system


sre : System -> System
sre system =
    system


tas : System -> System
tas system =
    system


xaa : System -> System
xaa system =
    system



-- STACK


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPush : System -> Int -> System
stackPush ({ cpu, memory } as system) value =
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
stackPull ({ cpu, memory } as system) =
    ( { system | cpu = { cpu | sp = cpu.sp + 1 } }
    , Memory.read (cpu.sp + 1) memory
    )
