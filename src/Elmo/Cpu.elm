module Elmo.Cpu exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor
called 2A03. It can handle sound, having a pAPU (psuedo-Audio Processing
Unit). However, it lacks Binary Coded Decimal mode.
-}

import Elmo.Cpu.Opcode as Opcode exposing (AddressingMode(..), Label(..))
import Elmo.Cpu.Instruction as Instruction exposing (Instruction)
import Elmo.Cpu.Flags as Flags
import Elmo.Cpu.Stack as Stack
import Elmo.Cpu.Ram as Ram
import Elmo.Types exposing (System, Interrupt(..), Cpu)
import Elmo.Utils exposing ((&&&), (|||), (^^^), (<<<), (>>>), pageCrossed, count)


init : Int -> Cpu
init pc =
    { pc = pc
    , sp = 0xFD
    , a = 0
    , x = 0
    , y = 0
    , p = Flags.init
    , interrupt = Nothing
    , stall = 0
    , cycles = 0
    }


tick : System -> System
tick ({ cpu, ram } as system) =
    if cpu.stall > 0 then
        { system | cpu = { cpu | stall = cpu.stall - 1 } }
    else
        let
            instruction =
                system |> Instruction.dispatch
        in
            system
                |> processInterrupt
                |> processCounters instruction
                |> processInstruction instruction



-- INTERNAL


processInterrupt : System -> System
processInterrupt ({ cpu } as system) =
    case cpu.interrupt of
        Nothing ->
            system

        Just NMI ->
            system |> nmi

        Just IRQ ->
            system |> irq


processCounters : Instruction -> System -> System
processCounters instruction ({ cpu } as system) =
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


processInstruction : Instruction -> System -> System
processInstruction instruction system =
    {- TODO(genadi): May wanna switch the arguments, so we don't need to flip
       the instructions while chaining them like in BRK.
    -}
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

        BRK ->
            instruction |> brk system

        BVC ->
            instruction |> bvc system

        BVS ->
            instruction |> bvs system

        CLC ->
            instruction |> clc system

        CLD ->
            instruction |> cld system

        CLI ->
            instruction |> cli system

        CLV ->
            instruction |> clv system

        CMP ->
            instruction |> cmp system

        CPX ->
            instruction |> cpx system

        CPY ->
            instruction |> cpy system

        DEC ->
            instruction |> dec system

        DEX ->
            instruction |> dex system

        DEY ->
            instruction |> dey system

        EOR ->
            instruction |> eor system

        INC ->
            instruction |> inc system

        INX ->
            instruction |> inx system

        INY ->
            instruction |> iny system

        JMP ->
            instruction |> jmp system

        JSR ->
            instruction |> jsr system

        LDA ->
            instruction |> lda system

        LDX ->
            instruction |> ldx system

        LDY ->
            instruction |> ldy system

        LSR ->
            instruction |> lsr system

        ORA ->
            instruction |> ora system

        PHA ->
            instruction |> pha system

        PHP ->
            instruction |> php system

        PLA ->
            instruction |> pla system

        PLP ->
            instruction |> plp system

        ROL ->
            instruction |> rol system

        ROR ->
            instruction |> ror system

        RTI ->
            instruction |> rti system

        RTS ->
            instruction |> rts system

        SBC ->
            instruction |> sbc system

        SEC ->
            instruction |> sec system

        SED ->
            instruction |> sed system

        SEI ->
            instruction |> sei system

        STA ->
            instruction |> sta system

        STX ->
            instruction |> stx system

        STY ->
            instruction |> sty system

        TAX ->
            instruction |> tax system

        TAY ->
            instruction |> tay system

        TSX ->
            instruction |> tsx system

        TXA ->
            instruction |> txa system

        TXS ->
            instruction |> txs system

        TYA ->
            instruction |> tya system

        NOP ->
            instruction |> nop system

        {- This matches all the illegal instructions. -}
        ILL ->
            instruction |> ill system

        {- Instructions that are unused on 2A03, but still present on 6502. -}
        ANC ->
            instruction |> anc system

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



-- INTERRUPTS


{-| Non-maskable interrupt.
-}
nmi : System -> System
nmi ({ cpu, ram } as system) =
    let
        systemAfterPush =
            system
                |> Stack.push16 cpu.pc
                |> Stack.push cpu.p
    in
        { systemAfterPush
            | cpu =
                { cpu
                    | pc = ram |> Ram.read16 0xFFFA
                    , p = cpu.p ||| Flags.interrupt
                    , cycles = cpu.cycles + 7
                }
        }


{-| IRQ interrupt.
-}
irq : System -> System
irq ({ cpu, ram } as system) =
    let
        systemAfterPush =
            system
                |> Stack.push16 cpu.pc
                |> Stack.push cpu.p
    in
        { systemAfterPush
            | cpu =
                { cpu
                    | pc = ram |> Ram.read16 0xFFFE
                    , p = cpu.p ||| Flags.interrupt
                    , cycles = cpu.cycles + 7
                }
        }



-- INSTRUCTIONS


{-| Add accumulator with carry.

See http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html on how
to handle overflows correctly.

Citing the article above:

Overflow can be computed simply in C++ from the inputs and the result. Overflow
occurs if (M^result)&(N^result)&0x80 is nonzero. That is, if the sign of both
inputs is different from the sign of the result.
-}
adc : System -> Instruction -> System
adc ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

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


{-| Logical and with ram address and accumulator.
-}
and : System -> Instruction -> System
and ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

        accumulator =
            cpu.a &&& value

        flags =
            cpu.p
                |> Flags.setSign accumulator
                |> Flags.setZero accumulator
    in
        { system | cpu = { cpu | a = accumulator, p = flags } }


{-| Shift Left One Bit (Ram or Accumulator).
-}
asl : System -> Instruction -> System
asl ({ cpu, ram } as system) { mode, address } =
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
                    ram |> Ram.read address
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
                    , ram =
                        ram |> Ram.write address (value <<< 1)
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


{-| Test bits in ram with accumulator.
-}
bit : System -> Instruction -> System
bit ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address
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


{-| Force break, a software interrupt.
-}
brk : System -> Instruction -> System
brk ({ cpu, ram } as system) instruction =
    let
        systemAfterStackPush =
            system
                |> Stack.push16 cpu.pc
                |> (flip php) instruction
                |> (flip sei) instruction
    in
        { systemAfterStackPush
            | cpu =
                { cpu
                    | pc =
                        ram |> Ram.read16 0xFFFE
                }
        }


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
clc ({ cpu } as system) { address } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setCarry False } }


{-| Clear decimal flag.
-}
cld : System -> Instruction -> System
cld ({ cpu } as system) { address } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setDecimal False } }


{-| Clear interrupt flag.
-}
cli : System -> Instruction -> System
cli ({ cpu } as system) { address } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setInterrupt False } }


{-| Clear overflow flag.
-}
clv : System -> Instruction -> System
clv ({ cpu } as system) { address } =
    { system | cpu = { cpu | p = cpu.p |> Flags.setOverflow False } }


{-| Compare ram and accumulator.
-}
cmp : System -> Instruction -> System
cmp ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

        div =
            (cpu.a - value) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setCarry (value < 0x0100)
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Compare ram and register X.
-}
cpx : System -> Instruction -> System
cpx ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

        div =
            (cpu.x - value) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setCarry (cpu.x >= value)
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Compare ram and register Y.
-}
cpy : System -> Instruction -> System
cpy ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

        div =
            (cpu.y - value) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setCarry (cpu.y >= value)
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Decrement ram value by one.
-}
dec : System -> Instruction -> System
dec ({ cpu, ram } as system) { address } =
    let
        value =
            ((ram |> Ram.read address) - 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Decrement register X value by 1.
-}
dex : System -> Instruction -> System
dex ({ cpu } as system) { address } =
    let
        value =
            (cpu.x - 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | x = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Decrement register Y value by 1.
-}
dey : System -> Instruction -> System
dey ({ cpu } as system) { address } =
    let
        value =
            (cpu.y - 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | y = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Exclusive or ram with accumulator.
-}
eor : System -> Instruction -> System
eor ({ cpu, ram } as system) { address } =
    let
        value =
            (ram |> Ram.read address) ^^^ cpu.a
    in
        { system
            | cpu =
                { cpu
                    | a = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Increment ram value by one.
-}
inc : System -> Instruction -> System
inc ({ cpu, ram } as system) { address } =
    let
        value =
            ((ram |> Ram.read address) + 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Increment register X by one.
-}
inx : System -> Instruction -> System
inx ({ cpu } as system) { address } =
    let
        value =
            (cpu.x + 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | x = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Increment register X by one.
-}
iny : System -> Instruction -> System
iny ({ cpu } as system) { address } =
    let
        value =
            (cpu.y + 1) &&& 0xFF
    in
        { system
            | cpu =
                { cpu
                    | y = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Jump to new location.
-}
jmp : System -> Instruction -> System
jmp ({ cpu, ram } as system) { address } =
    { system | cpu = { cpu | pc = ram |> Ram.read address } }


{-| Jump to new location saving return address.
-}
jsr : System -> Instruction -> System
jsr ({ cpu, ram } as system) { address } =
    let
        systemAfterPush =
            system |> Stack.push16 (cpu.pc - 1)
    in
        { systemAfterPush | cpu = { cpu | pc = address } }


{-| Load ram to the accumulator..
-}
lda : System -> Instruction -> System
lda ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address
    in
        { system
            | cpu =
                { cpu
                    | a = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Load ram to register X.
-}
ldx : System -> Instruction -> System
ldx ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address
    in
        { system
            | cpu =
                { cpu
                    | x = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Load ram to register Y.
-}
ldy : System -> Instruction -> System
ldy ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address
    in
        { system
            | cpu =
                { cpu
                    | y = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Shift Left One Bit (Ram or Accumulator).
-}
lsr : System -> Instruction -> System
lsr ({ cpu, ram } as system) { mode, address } =
    case mode of
        Accumulator ->
            { system
                | cpu =
                    { cpu
                        | a = cpu.a >>> 1
                        , p =
                            cpu.p
                                |> Flags.setCarry ((cpu.a &&& 0x01) == 0x01)
                                |> Flags.setSign cpu.a
                                |> Flags.setZero cpu.a
                    }
            }

        _ ->
            let
                value =
                    ram |> Ram.read address
            in
                { system
                    | cpu =
                        { cpu
                            | p =
                                cpu.p
                                    |> Flags.setCarry ((value &&& 0x01) == 0x01)
                                    |> Flags.setSign value
                                    |> Flags.setZero value
                        }
                    , ram =
                        ram |> Ram.write address (value >>> 1)
                }


{-| Logical inclusive or between ram and accumulator.
-}
ora : System -> Instruction -> System
ora ({ cpu, ram } as system) { address } =
    let
        value =
            cpu.a ||| (ram |> Ram.read address)
    in
        { system
            | cpu =
                { cpu
                    | a = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Push the accumulator to the stack.
-}
pha : System -> Instruction -> System
pha ({ cpu } as system) instruction =
    system |> Stack.push cpu.a


{-| Push the status register to the stack.
-}
php : System -> Instruction -> System
php ({ cpu } as system) instruction =
    system |> Stack.push cpu.p


{-| Pull accumulator from the stack.
-}
pla : System -> Instruction -> System
pla ({ cpu } as system) instruction =
    let
        ( systemAfterPull, value ) =
            system |> Stack.pull
    in
        { systemAfterPull
            | cpu =
                { cpu
                    | a = value
                    , p =
                        cpu.p
                            |> Flags.setSign value
                            |> Flags.setZero value
                }
        }


{-| Pull status from the stack.
-}
plp : System -> Instruction -> System
plp ({ cpu } as system) instruction =
    let
        ( systemAfterPull, value ) =
            system |> Stack.pull
    in
        { systemAfterPull | cpu = { cpu | p = value } }


{-| Rotate ram or accumulator one bit left.
-}
rol : System -> Instruction -> System
rol ({ cpu, ram } as system) { address, mode } =
    case mode of
        Accumulator ->
            let
                value =
                    (cpu.a <<< 1) ||| (cpu.p &&& Flags.carry)
            in
                { system
                    | cpu =
                        { cpu
                            | a = value
                            , p =
                                cpu.p
                                    |> Flags.setCarry (value > 0xFF)
                                    |> Flags.setSign value
                                    |> Flags.setZero value
                        }
                }

        _ ->
            let
                value =
                    ((ram |> Ram.read address) <<< 1)
                        ||| (cpu.p &&& Flags.carry)
            in
                { system
                    | cpu =
                        { cpu
                            | p =
                                cpu.p
                                    |> Flags.setCarry (value > 0xFF)
                                    |> Flags.setSign value
                                    |> Flags.setZero value
                        }
                    , ram =
                        ram |> Ram.write address value
                }


{-| Rotate ram or accumulator one bit right.
-}
ror : System -> Instruction -> System
ror ({ cpu, ram } as system) { address, mode } =
    case mode of
        Accumulator ->
            let
                value =
                    if (cpu.p &&& Flags.carry) == Flags.carry then
                        cpu.a ||| 0x0100
                    else
                        cpu.a

                carry =
                    (cpu.a &&& (cpu.p &&& Flags.carry)) == Flags.carry
            in
                { system
                    | cpu =
                        { cpu
                            | a = value >>> 1
                            , p =
                                cpu.p
                                    |> Flags.setCarry carry
                                    |> Flags.setSign (value >>> 1)
                                    |> Flags.setZero (value >>> 1)
                        }
                }

        _ ->
            let
                raw =
                    ram |> Ram.read address

                value =
                    if (cpu.p &&& Flags.carry) == Flags.carry then
                        raw ||| 0x0100
                    else
                        raw

                carry =
                    (raw &&& (cpu.p &&& Flags.carry)) == Flags.carry
            in
                { system
                    | cpu =
                        { cpu
                            | p =
                                cpu.p
                                    |> Flags.setCarry carry
                                    |> Flags.setSign (value >>> 1)
                                    |> Flags.setZero (value >>> 1)
                        }
                    , ram =
                        ram |> Ram.write address (value >>> 1)
                }


{-| Return from interrupt.
-}
rti : System -> Instruction -> System
rti ({ cpu } as system) instruction =
    let
        ( systemAfterPull, status ) =
            system |> Stack.pull

        ( systemAfterDoublePull, pc ) =
            system |> Stack.pull16
    in
        { systemAfterDoublePull
            | cpu =
                { cpu
                    | p = status
                    , pc = pc
                }
        }


{-| Return from subroutine.
-}
rts : System -> Instruction -> System
rts ({ cpu } as system) instruction =
    let
        ( systemAfterDoublePull, pc ) =
            system |> Stack.pull16
    in
        { systemAfterDoublePull | cpu = { cpu | pc = pc } }


{-| Subtract ram from accumulator with borrow.
-}
sbc : System -> Instruction -> System
sbc ({ cpu, ram } as system) { address } =
    let
        value =
            ram |> Ram.read address

        sub =
            cpu.a - value - (1 - (cpu.p &&& Flags.carry))

        accumulator =
            0xFF &&& sub

        overflow =
            ((cpu.a ^^^ sub) &&& (value ^^^ sub) &&& 0x80) /= 0

        flags =
            cpu.p
                |> Flags.setSign accumulator
                |> Flags.setZero accumulator
                |> Flags.setCarry (sub < 0)
                |> Flags.setOverflow overflow
    in
        { system | cpu = { cpu | a = accumulator, p = flags } }


{-| Set carry flag.
-}
sec : System -> Instruction -> System
sec ({ cpu } as system) instruction =
    { system | cpu = { cpu | p = cpu.p ||| Flags.carry } }


{-| Set decimal flag.
-}
sed : System -> Instruction -> System
sed ({ cpu } as system) instruction =
    { system | cpu = { cpu | p = cpu.p ||| Flags.decimal } }


{-| Set decimal flag.
-}
sei : System -> Instruction -> System
sei ({ cpu } as system) instruction =
    { system | cpu = { cpu | p = cpu.p ||| Flags.interrupt } }


{-| Store accumulator in ram.
-}
sta : System -> Instruction -> System
sta ({ cpu, ram } as system) { address } =
    { system | ram = ram |> Ram.write address cpu.a }


{-| Store register X in ram.
-}
stx : System -> Instruction -> System
stx ({ cpu, ram } as system) { address } =
    { system | ram = ram |> Ram.write address cpu.x }


{-| Store register Y in ram.
-}
sty : System -> Instruction -> System
sty ({ cpu, ram } as system) { address } =
    { system | ram = ram |> Ram.write address cpu.y }


{-| Transfer accumulator to register X.
-}
tax : System -> Instruction -> System
tax ({ cpu } as system) { address } =
    { system
        | cpu =
            { cpu
                | x = cpu.a
                , p =
                    cpu.p
                        |> Flags.setSign cpu.a
                        |> Flags.setZero cpu.a
            }
    }


{-| Transfer accumulator to register Y.
-}
tay : System -> Instruction -> System
tay ({ cpu } as system) { address } =
    { system
        | cpu =
            { cpu
                | y = cpu.a
                , p =
                    cpu.p
                        |> Flags.setSign cpu.a
                        |> Flags.setZero cpu.a
            }
    }


{-| Transfer stack pointer to register X.
-}
tsx : System -> Instruction -> System
tsx ({ cpu } as system) { address } =
    { system
        | cpu =
            { cpu
                | x = cpu.sp
                , p =
                    cpu.p
                        |> Flags.setSign cpu.sp
                        |> Flags.setZero cpu.sp
            }
    }


{-| Transfer register X to accumulator.
-}
txa : System -> Instruction -> System
txa ({ cpu } as system) { address } =
    { system
        | cpu =
            { cpu
                | a = cpu.x
                , p =
                    cpu.p
                        |> Flags.setSign cpu.x
                        |> Flags.setZero cpu.x
            }
    }


{-| Transfer register X to stack pointer.
-}
txs : System -> Instruction -> System
txs ({ cpu } as system) { address } =
    { system | cpu = { cpu | sp = cpu.x } }


{-| Transfer register Y to accumulator.
-}
tya : System -> Instruction -> System
tya ({ cpu } as system) { address } =
    { system
        | cpu =
            { cpu
                | a = cpu.y
                , p =
                    cpu.p
                        |> Flags.setSign cpu.y
                        |> Flags.setZero cpu.y
            }
    }


{-| No-operation instruction.

It simply does nothing. Useful to comment code in assembly.
-}
nop : System -> Instruction -> System
nop system instruction =
    system


{-| This handle illegal instructions.

For now they are no-operations, but I may do something else in the future.
-}
ill : System -> Instruction -> System
ill =
    nop



-- UNUSED


anc =
    nop


ahx =
    nop


alr =
    nop


arr =
    nop


axs =
    nop


dcp =
    nop


isc =
    nop


las =
    nop


lax =
    nop


rla =
    nop


rra =
    nop


sax =
    nop


shx =
    nop


shy =
    nop


slo =
    nop


sre =
    nop


tas =
    nop


xaa =
    nop
