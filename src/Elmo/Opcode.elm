module Elmo.Opcode exposing (..)

{-| Instruction dispatching code for the 2A03 CPU.

# Documentation

- http://www.thealmightyguru.com/Games/Hacking/Wiki/index.php/6502_Opcodes
- http://obelisk.me.uk/6502/reference.html
-}


type AddressingMode
    = Absolute
    | AbsoluteX
    | AbsoluteY
    | Accumulator
    | Relative
    | Indirect
    | IndirectX
    | IndirectY
    | Immediate
    | Implied
    | ZeroPage
    | ZeroPageX
    | ZeroPageY


type InstructionLabel
    = ADC
    | AHX
    | ALR
    | ANC
    | AND
    | ARR
    | ASL
    | AXS
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DCP
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | ISC
    | JMP
    | JSR
    | LAS
    | LAX
    | LDA
    | LDX
    | LDY
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | RLA
    | ROL
    | ROR
    | RRA
    | RTI
    | RTS
    | SAX
    | SBC
    | SEC
    | SED
    | SEI
    | SHX
    | SHY
    | SLO
    | SRE
    | STA
    | STX
    | STY
    | TAS
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA
    | XAA
    | ILL


type alias Instruction =
    { label : InstructionLabel
    , mode : AddressingMode
    , opcode : Int
    , cycles : Int
    , pageCycles : Int
    }


dispatch : Int -> Instruction
dispatch opcode =
    case opcode of
        0x00 ->
            Instruction BRK Implied opcode 7 0

        0x01 ->
            Instruction ORA IndirectX opcode 6 0

        0x05 ->
            Instruction ORA ZeroPage opcode 3 0

        0x06 ->
            Instruction ASL ZeroPage opcode 5 0

        0x08 ->
            Instruction PHP Implied opcode 3 0

        0x09 ->
            Instruction ORA Immediate opcode 2 0

        0x0A ->
            Instruction ASL Accumulator opcode 2 0

        0x0D ->
            Instruction ORA Absolute opcode 4 0

        0x0E ->
            Instruction ASL Absolute opcode 6 0

        0x10 ->
            Instruction BPL Immediate opcode 2 0

        0x11 ->
            Instruction ORA IndirectY opcode 5 1

        0x15 ->
            Instruction ORA ZeroPageX opcode 4 0

        0x16 ->
            Instruction ASL ZeroPageX opcode 6 0

        0x18 ->
            Instruction CLC Immediate opcode 2 0

        0x19 ->
            Instruction ORA AbsoluteY opcode 4 1

        0x1D ->
            Instruction ORA AbsoluteX opcode 4 1

        0x1E ->
            Instruction ASL AbsoluteX opcode 7 0

        0x20 ->
            Instruction JSR Absolute opcode 6 0

        0x21 ->
            Instruction AND IndirectX opcode 6 0

        0x24 ->
            Instruction BIT ZeroPage opcode 3 0

        0x25 ->
            Instruction AND ZeroPage opcode 3 0

        0x26 ->
            Instruction ROL ZeroPage opcode 5 0

        0x28 ->
            Instruction PLP Implied opcode 4 0

        0x29 ->
            Instruction AND Immediate opcode 2 0

        0x2A ->
            Instruction ROL Accumulator opcode 2 0

        0x2C ->
            Instruction BIT Absolute opcode 4 0

        0x2D ->
            Instruction AND Absolute opcode 4 0

        0x2E ->
            Instruction ROL Absolute opcode 6 0

        0x30 ->
            Instruction BMI Immediate opcode 2 0

        0x31 ->
            Instruction AND IndirectY opcode 5 1

        0x35 ->
            Instruction AND ZeroPageX opcode 4 0

        0x36 ->
            Instruction ROL ZeroPageX opcode 6 0

        0x38 ->
            Instruction SEC Implied opcode 2 0

        0x39 ->
            Instruction AND AbsoluteY opcode 4 1

        0x3D ->
            Instruction AND AbsoluteX opcode 4 1

        0x3E ->
            Instruction ROL AbsoluteX opcode 7 0

        0x40 ->
            Instruction RTI Implied opcode 6 0

        0x41 ->
            Instruction EOR IndirectX opcode 6 0

        0x45 ->
            Instruction EOR ZeroPage opcode 3 0

        0x46 ->
            Instruction LSR ZeroPage opcode 5 0

        0x48 ->
            Instruction PHA Implied opcode 3 0

        0x49 ->
            Instruction EOR Immediate opcode 2 0

        0x4A ->
            Instruction LSR Accumulator opcode 2 0

        0x4C ->
            Instruction JMP Absolute opcode 3 0

        0x4D ->
            Instruction EOR Absolute opcode 4 0

        0x4E ->
            Instruction LSR Absolute opcode 6 0

        0x50 ->
            Instruction BVC Relative opcode 2 0

        0x51 ->
            Instruction EOR IndirectY opcode 5 1

        0x55 ->
            Instruction EOR ZeroPageX opcode 4 0

        0x56 ->
            Instruction LSR ZeroPageX opcode 6 0

        0x58 ->
            Instruction CLI Implied opcode 2 0

        0x59 ->
            Instruction EOR AbsoluteY opcode 4 1

        0x5D ->
            Instruction EOR AbsoluteX opcode 4 1

        0x5E ->
            Instruction LSR AbsoluteX opcode 7 0

        0x60 ->
            Instruction RTS Implied opcode 5 0

        0x61 ->
            Instruction ADC IndirectX opcode 6 0

        0x65 ->
            Instruction ADC ZeroPage opcode 3 0

        0x66 ->
            Instruction ROR ZeroPage opcode 5 0

        0x68 ->
            Instruction PLA Implied opcode 4 0

        0x69 ->
            Instruction ADC Immediate opcode 2 0

        0x6A ->
            Instruction ROR Accumulator opcode 2 0

        0x6C ->
            Instruction JMP Indirect opcode 5 0

        0x6D ->
            Instruction ADC Absolute opcode 4 0

        0x6E ->
            Instruction ROR Absolute opcode 6 0

        0x70 ->
            Instruction BVS Relative opcode 2 0

        0x71 ->
            Instruction ADC IndirectY opcode 5 1

        0x75 ->
            Instruction ADC ZeroPageX opcode 4 0

        0x76 ->
            Instruction ROR ZeroPageX opcode 6 0

        0x78 ->
            Instruction SEI Implied opcode 2 0

        0x79 ->
            Instruction ADC AbsoluteY opcode 4 1

        0x7D ->
            Instruction ADC AbsoluteX opcode 4 1

        0x7E ->
            Instruction ROR AbsoluteX opcode 7 0

        0x81 ->
            Instruction STA IndirectX opcode 6 0

        0x84 ->
            Instruction STY ZeroPage opcode 3 0

        0x85 ->
            Instruction STA ZeroPage opcode 3 0

        0x86 ->
            Instruction STX ZeroPage opcode 3 0

        0x88 ->
            Instruction DEY Implied opcode 2 0

        0x8A ->
            Instruction TXA Immediate opcode 2 0

        0x8C ->
            Instruction STY Absolute opcode 4 0

        0x8D ->
            Instruction STA Absolute opcode 4 0

        0x8E ->
            Instruction STX Absolute opcode 4 0

        0x90 ->
            Instruction BCC Relative opcode 2 0

        0x91 ->
            Instruction STA IndirectY opcode 6 0

        0x94 ->
            Instruction STY ZeroPageX opcode 4 0

        0x95 ->
            Instruction STA ZeroPageX opcode 4 0

        0x96 ->
            Instruction STX ZeroPageY opcode 4 0

        0x98 ->
            Instruction TYA Implied opcode 2 0

        0x99 ->
            Instruction STA AbsoluteY opcode 5 0

        0x9A ->
            Instruction TXS Implied opcode 2 0

        0x9D ->
            Instruction STA AbsoluteX opcode 5 0

        0xA0 ->
            Instruction LDY Immediate opcode 2 0

        0xA1 ->
            Instruction LDA IndirectX opcode 6 0

        0xA2 ->
            Instruction LDX Immediate opcode 2 0

        0xA4 ->
            Instruction LDY ZeroPage opcode 3 0

        0xA5 ->
            Instruction LDA ZeroPage opcode 3 0

        0xA6 ->
            Instruction LDX ZeroPage opcode 3 0

        0xA8 ->
            Instruction TAY Implied opcode 2 0

        0xA9 ->
            Instruction LDA Immediate opcode 2 0

        0xAA ->
            Instruction TAX Implied opcode 2 0

        0xAC ->
            Instruction LDY Absolute opcode 4 0

        0xAD ->
            Instruction LDA Absolute opcode 4 0

        0xAE ->
            Instruction LDX Absolute opcode 4 0

        0xB0 ->
            Instruction BCS Relative opcode 2 0

        0xB1 ->
            Instruction LDA IndirectY opcode 5 1

        0xB4 ->
            Instruction LDY ZeroPageX opcode 4 0

        0xB5 ->
            Instruction LDA ZeroPageX opcode 4 0

        0xB6 ->
            Instruction LDX ZeroPageY opcode 4 0

        0xB8 ->
            Instruction CLV Implied opcode 2 0

        0xB9 ->
            Instruction LDA AbsoluteY opcode 4 1

        0xBA ->
            Instruction TSX Implied opcode 2 0

        0xBC ->
            Instruction LDY AbsoluteX opcode 4 1

        0xBD ->
            Instruction LDA AbsoluteX opcode 4 1

        0xBE ->
            Instruction LDX AbsoluteY opcode 4 1

        0xC0 ->
            Instruction CPY Immediate opcode 2 0

        0xC1 ->
            Instruction CMP IndirectX opcode 6 0

        0xC4 ->
            Instruction CPY ZeroPage opcode 3 0

        0xC5 ->
            Instruction CMP ZeroPage opcode 3 0

        0xC6 ->
            Instruction DEC ZeroPage opcode 5 0

        0xC8 ->
            Instruction INY Implied opcode 2 0

        0xC9 ->
            Instruction CMP Immediate opcode 2 0

        0xCA ->
            Instruction DEX Implied opcode 2 0

        0xCC ->
            Instruction CPY Absolute opcode 4 0

        0xCD ->
            Instruction CMP Absolute opcode 4 0

        0xCE ->
            Instruction DEC Absolute opcode 6 0

        0xD0 ->
            Instruction BNE Immediate opcode 2 0

        0xD1 ->
            Instruction CMP IndirectY opcode 5 1

        0xD5 ->
            Instruction CMP ZeroPageX opcode 4 0

        0xD6 ->
            Instruction DEC ZeroPageX opcode 6 0

        0xD8 ->
            Instruction CLD Immediate opcode 2 0

        0xD9 ->
            Instruction CMP AbsoluteY opcode 4 1

        0xDD ->
            Instruction CMP AbsoluteX opcode 4 1

        0xDE ->
            Instruction DEC AbsoluteX opcode 7 0

        0xE0 ->
            Instruction CPX Immediate opcode 2 0

        0xE1 ->
            Instruction SBC IndirectX opcode 6 0

        0xE4 ->
            Instruction CPX ZeroPage opcode 3 0

        0xE5 ->
            Instruction SBC ZeroPage opcode 3 0

        0xE6 ->
            Instruction INC ZeroPage opcode 5 0

        0xE8 ->
            Instruction INX Implied opcode 2 0

        0xE9 ->
            Instruction SBC Immediate opcode 2 0

        0xEA ->
            Instruction NOP Immediate opcode 2 0

        0xEC ->
            Instruction CPX Absolute opcode 4 0

        0xED ->
            Instruction SBC Absolute opcode 4 0

        0xEE ->
            Instruction INC Absolute opcode 6 0

        0xF0 ->
            Instruction BEQ Relative opcode 2 0

        0xF1 ->
            Instruction SBC IndirectY opcode 5 1

        0xF5 ->
            Instruction SBC ZeroPageX opcode 4 0

        0xF6 ->
            Instruction INC ZeroPageX opcode 6 0

        0xF8 ->
            Instruction SED Implied opcode 2 0

        0xF9 ->
            Instruction SBC AbsoluteY opcode 4 1

        0xFD ->
            Instruction SBC AbsoluteX opcode 4 1

        0xFE ->
            Instruction INC AbsoluteX opcode 7 0

        _ ->
            Instruction ILL Implied opcode 0 0
