module Elmo.Cpu exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Memory as Memory exposing (Memory)
import Elmo.Opcode as Opcode exposing (AddressingMode(..), Label(..))
import Elmo.Instruction as Instruction exposing (Instruction)
import Elmo.Types exposing (System, Interrupt, Cpu)
import Elmo.Utils exposing ((&&&), (|||), (^^^), (<<<), pageCrossed, count)
import Elmo.Flags as Flags
import Bitwise


tick : System -> System
tick ({ cpu, memory } as system) =
    if cpu.stall > 0 then
        { system | cpu = { cpu | stall = cpu.stall - 1 } }
    else
        let
            instruction =
                Instruction.dispatch system

            newSystem =
                { system
                    | cpu =
                        { cpu
                            | pc = cpu.pc + instruction.bytes
                            , cycles =
                                cpu.cycles
                                    + instruction.cycles
                                    + (count instruction.pageCrossed)
                        }
                }
        in
            {- We still need to handle interrupts here. -}
            instruction |> process newSystem


process : System -> Instruction -> System
process system instruction =
    case instruction.label of
        ADC ->
            instruction |> adc system

        AND ->
            instruction |> and system

        ASL ->
            instruction |> asl system

        BCC ->
            instruction |> bcc system

        BCS ->
            instruction |> bcs system

        BEQ ->
            instruction |> beq system

        BIT ->
            instruction |> bit system

        BMI ->
            instruction |> bmi system

        BNE ->
            instruction |> bne system

        BPL ->
            instruction |> bpl system

        BVC ->
            instruction |> bvc system

        BVS ->
            instruction |> bvs system

        CLC ->
            instruction |> clc system

        CLD ->
            instruction |> cld system

        NOP ->
            instruction |> nop system

        {- Instructions that are unused on 2A03, but still present on 6502. -}
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



-- INSTRUCTION


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


{-| Branch if no carry flag is set.
-}
bcc : System -> Instruction -> System
bcc ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.carry) == 0 then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch if carry flag is set.
-}
bcs : System -> Instruction -> System
bcs ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.carry) == Flags.carry then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch if result is zero.
-}
beq : System -> Instruction -> System
beq ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.zero) == Flags.zero then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Test bits in memory with accumulator.
-}
bit : System -> Instruction -> System
bit ({ cpu, memory } as system) { address } =
    let
        value =
            memory |> Memory.read address
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setOverflow ((0x40 &&& value) == 0x40)
                            |> Flags.setZero (value &&& cpu.a)
                }
        }


{-| Branch on result minus.
-}
bmi : System -> Instruction -> System
bmi ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.sign) == Flags.sign then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch on result not zero.
-}
bne : System -> Instruction -> System
bne ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.zero) == 0 then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch on positive result.
-}
bpl : System -> Instruction -> System
bpl ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.sign) == 0 then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch on overflow zero.
-}
bvc : System -> Instruction -> System
bvc ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.overflow) == 0 then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Branch on overflow set.
-}
bvs : System -> Instruction -> System
bvs ({ cpu } as system) { address, branchPageCycles } =
    if (cpu.p &&& Flags.overflow) == Flags.overflow then
        { system
            | cpu =
                { cpu
                    | pc = address
                    , cycles =
                        cpu.cycles
                            + branchPageCycles
                            + count (pageCrossed cpu.pc address)
                }
        }
    else
        system


{-| Clear carry flag.
-}
clc : System -> Instruction -> System
clc ({ cpu } as system) { address, branchPageCycles } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setCarry False } }


{-| Clear carry flag.
-}
cld : System -> Instruction -> System
cld ({ cpu } as system) { address, branchPageCycles } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setDecimal False } }


{-| No-operation instruction.

It simply does nothing. Useful to comment code in assembly.
-}
nop : System -> Instruction -> System
nop system instruction =
    system



-- UNUSED


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
