module Elmo.Core exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Memory as Memory exposing (Memory)
import Elmo.Opcode as Opcode exposing (AddressingMode(..), Opcode, Label(..))
import Elmo.Flags as Flags exposing (..)
import Elmo.Math exposing ((&&&), (|||), (^^^), (<<<))
import Bitwise


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
                            , cycles = cpu |> updateCycles instruction
                        }
                }
        in
            {- We still need to handle interrupts here. -}
            instruction |> processInstruction newSystem


updateCycles : Instruction -> Cpu -> Int
updateCycles instruction { cycles } =
    cycles
        + instruction.cycles
        + (if instruction.pageCrossed then
            instruction.pageCycles
           else
            0
          )



-- INSTRUCTION


type alias RuntimeAugmentation opcode =
    { opcode
        | address : Int
        , pageCrossed : Bool
    }


type alias Instruction =
    RuntimeAugmentation Opcode


dispatchInstruction : System -> Instruction
dispatchInstruction { cpu, memory } =
    let
        pageCrossed : Int -> Int -> Bool
        pageCrossed a b =
            (0xFF00 &&& a) /= (0xFF00 &&& b)

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
                        memory |> Memory.read (0xFF &&& (cpu.pc + 1 + cpu.x))
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


processInstruction : System -> Instruction -> System
processInstruction system instruction =
    case instruction.label of
        ADC ->
            instruction |> adc system

        AND ->
            instruction |> and system

        ASL ->
            instruction |> asl system

        NOP ->
            instruction |> nop system

        AHX ->
            instruction |> ahx system

        ALR ->
            instruction |> alr system

        ARR ->
            instruction |> arr system

        AXS ->
            instruction |> axs system

        DCP ->
            instruction |> dcp system

        ILL ->
            instruction |> ill system

        ISC ->
            instruction |> isc system

        LAS ->
            instruction |> las system

        LAX ->
            instruction |> lax system

        RLA ->
            instruction |> rla system

        RRA ->
            instruction |> rra system

        SAX ->
            instruction |> sax system

        SHX ->
            instruction |> shx system

        SHY ->
            instruction |> shy system

        SLO ->
            instruction |> slo system

        SRE ->
            instruction |> sre system

        TAS ->
            instruction |> tas system

        XAA ->
            instruction |> xaa system

        _ ->
            system


augmentToInstruction : { address : Int, pageCrossed : Bool } -> Opcode -> Instruction
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


{-| Add accumulator with carry.

See http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html on how
to handle overflows correctly.

Citing the article above:

Overflow can be computed simply in C++ from the inputs and the result. Overflow
occurs if (M^result)&(N^result)&0x80 is nonzero. That is, if the sign of both
inputs is different from the sign of the result.
-}
adc : System -> Instruction -> System
adc ({ cpu, memory } as system) { address } =
    let
        value =
            memory |> Memory.read address

        sum =
            cpu.a + value + (cpu.p &&& Flags.carry)

        accumulator =
            0xFF &&& sum

        overflow =
            ((cpu.a ^^^ sum) &&& (value ^^^ sum) &&& 0x80) /= 0

        flags =
            cpu.p
                |> Flags.setSign accumulator
                |> Flags.setZero sum
                |> Flags.setCarry (sum > 0xFF)
                |> Flags.setOverflow overflow
    in
        { system | cpu = { cpu | a = accumulator, p = flags } }


{-| Logical and with memory address and accumulator.
-}
and : System -> Instruction -> System
and ({ cpu, memory } as system) { address } =
    let
        value =
            memory |> Memory.read address

        accumulator =
            cpu.a &&& value

        flags =
            cpu.p
                |> Flags.setSign accumulator
                |> Flags.setZero accumulator
    in
        { system | cpu = { cpu | a = accumulator, p = flags } }


{-| Shift Left One Bit (Memory or Accumulator).
-}
asl : System -> Instruction -> System
asl ({ cpu, memory } as system) { mode, address } =
    case mode of
        Accumulator ->
            { system
                | cpu =
                    { cpu
                        | a = cpu.a <<< 1
                        , p =
                            cpu.p
                                |> Flags.setCarry ((cpu.a &&& 0x80) == 0x80)
                                |> Flags.setSign cpu.a
                                |> Flags.setZero cpu.a
                    }
            }

        _ ->
            let
                value =
                    memory |> Memory.read address
            in
                { system
                    | cpu =
                        { cpu
                            | p =
                                cpu.p
                                    |> Flags.setCarry ((value &&& 0x80) == 0x80)
                                    |> Flags.setSign value
                                    |> Flags.setZero value
                        }
                    , memory =
                        memory |> Memory.write address (value <<< 1)
                }


{-| No-operation instruction.

It simply does nothing. Useful to comment code in assembly.
-}
nop : System -> Instruction -> System
nop system instruction =
    system



{- The following instructions are unused on the 2A03, but are still present on
   the 6502.
-}


ahx : System -> Instruction -> System
ahx system instruction =
    system


alr : System -> Instruction -> System
alr system instruction =
    system


arr : System -> Instruction -> System
arr system instruction =
    system


axs : System -> Instruction -> System
axs system instruction =
    system


dcp : System -> Instruction -> System
dcp system instruction =
    system


ill : System -> Instruction -> System
ill system instruction =
    system


isc : System -> Instruction -> System
isc system instruction =
    system


las : System -> Instruction -> System
las system instruction =
    system


lax : System -> Instruction -> System
lax system instruction =
    system


rla : System -> Instruction -> System
rla system instruction =
    system


rra : System -> Instruction -> System
rra system instruction =
    system


sax : System -> Instruction -> System
sax system instruction =
    system


shx : System -> Instruction -> System
shx system instruction =
    system


shy : System -> Instruction -> System
shy system instruction =
    system


slo : System -> Instruction -> System
slo system instruction =
    system


sre : System -> Instruction -> System
sre system instruction =
    system


tas : System -> Instruction -> System
tas system instruction =
    system


xaa : System -> Instruction -> System
xaa system instruction =
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
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp - 1) }
        , memory = memory |> Memory.write cpu.sp value
    }


{-| For a detailed look on how the 6502 stack works, go to:
https://wiki.nesdev.com/w/index.php/Stack
-}
stackPull : System -> ( System, Int )
stackPull ({ cpu, memory } as system) =
    ( { system
        | cpu = { cpu | sp = 0x1FFF &&& (cpu.sp + 1) }
      }
    , Memory.read (cpu.sp + 1) memory
    )
