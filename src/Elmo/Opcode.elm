module Elmo.Opcode exposing (..)

{-| Instruction dispatching code based on documentation provided by
http://www.thealmightyguru.com/Games/Hacking/Wiki/index.php/6502_Instructions.
-}


type AddressingMode
    = Absolute
    | AbsoluteX
    | AbsoluteY
    | Accumulator
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
    }


dispatch : Int -> Instruction
dispatch opcode =
    case opcode of
        0x00 ->
            Instruction BRK Implied opcode

        0x01 ->
            Instruction ORA IndirectX opcode

        0x05 ->
            Instruction ORA ZeroPage opcode

        0x06 ->
            Instruction ASL ZeroPage opcode

        0x08 ->
            Instruction PHP Implied opcode

        0x09 ->
            Instruction ORA Immediate opcode

        0x0A ->
            Instruction ASL Accumulator opcode

        0x0D ->
            Instruction ORA Absolute opcode

        0x0E ->
            Instruction ASL Absolute opcode

        0x10 ->
            Instruction BPL Immediate opcode

        0x11 ->
            Instruction ORA IndirectY opcode

        0x15 ->
            Instruction ORA ZeroPageX opcode

        0x16 ->
            Instruction ASL ZeroPageX opcode

        0x18 ->
            Instruction CLC Immediate opcode

        0x19 ->
            Instruction ORA AbsoluteY opcode

        0x1D ->
            Instruction ORA AbsoluteX opcode

        0x1E ->
            Instruction ASL AbsoluteX opcode

        0x20 ->
            Instruction JSR Immediate opcode

        0x21 ->
            Instruction AND IndirectX opcode

        0x24 ->
            Instruction BIT ZeroPage opcode

        0x25 ->
            Instruction AND ZeroPage opcode

        0x26 ->
            Instruction ROL ZeroPage opcode

        0x28 ->
            Instruction PLP Immediate opcode

        0x29 ->
            Instruction AND Immediate opcode

        0x2A ->
            Instruction ROL Accumulator opcode

        0x2C ->
            Instruction BIT Absolute opcode

        0x2D ->
            Instruction AND Absolute opcode

        0x2E ->
            Instruction ROL Absolute opcode

        0x30 ->
            Instruction BMI Immediate opcode

        0x31 ->
            Instruction AND IndirectY opcode

        0x35 ->
            Instruction AND ZeroPageX opcode

        0x36 ->
            Instruction ROL ZeroPageX opcode

        0x38 ->
            Instruction SEC Immediate opcode

        0x39 ->
            Instruction AND AbsoluteY opcode

        0x3D ->
            Instruction AND AbsoluteX opcode

        0x3E ->
            Instruction ROL AbsoluteX opcode

        0x40 ->
            Instruction RTI Immediate opcode

        0x41 ->
            Instruction EOR IndirectX opcode

        0x45 ->
            Instruction EOR ZeroPage opcode

        0x46 ->
            Instruction LSR ZeroPage opcode

        0x48 ->
            Instruction PHA Immediate opcode

        0x49 ->
            Instruction EOR Immediate opcode

        0x4A ->
            Instruction LSR Accumulator opcode

        0x4C ->
            Instruction JMP Absolute opcode

        0x4D ->
            Instruction EOR Absolute opcode

        0x4E ->
            Instruction LSR Absolute opcode

        0x50 ->
            Instruction BVC Immediate opcode

        0x51 ->
            Instruction EOR IndirectY opcode

        0x55 ->
            Instruction EOR ZeroPageX opcode

        0x56 ->
            Instruction LSR ZeroPageX opcode

        0x58 ->
            Instruction CLI Immediate opcode

        0x59 ->
            Instruction EOR AbsoluteY opcode

        0x5D ->
            Instruction EOR AbsoluteX opcode

        0x5E ->
            Instruction LSR AbsoluteX opcode

        0x60 ->
            Instruction RTS Immediate opcode

        0x61 ->
            Instruction ADC IndirectX opcode

        0x65 ->
            Instruction ADC ZeroPage opcode

        0x66 ->
            Instruction ROR ZeroPage opcode

        0x68 ->
            Instruction PLA Immediate opcode

        0x69 ->
            Instruction ADC Immediate opcode

        0x6A ->
            Instruction ROR Accumulator opcode

        0x6C ->
            Instruction JMP Indirect opcode

        0x6D ->
            Instruction ADC Absolute opcode

        0x6E ->
            Instruction ROR Absolute opcode

        0x70 ->
            Instruction BVS Immediate opcode

        0x71 ->
            Instruction ADC IndirectY opcode

        0x75 ->
            Instruction ADC ZeroPageX opcode

        0x76 ->
            Instruction ROR ZeroPageX opcode

        0x78 ->
            Instruction SEI Immediate opcode

        0x79 ->
            Instruction ADC AbsoluteY opcode

        0x7D ->
            Instruction ADC AbsoluteX opcode

        0x7E ->
            Instruction ROR AbsoluteX opcode

        0x81 ->
            Instruction STA IndirectX opcode

        0x84 ->
            Instruction STY ZeroPage opcode

        0x85 ->
            Instruction STA ZeroPage opcode

        0x86 ->
            Instruction STX ZeroPage opcode

        0x88 ->
            Instruction DEY Immediate opcode

        0x8A ->
            Instruction TXA Immediate opcode

        0x8C ->
            Instruction STY Absolute opcode

        0x8D ->
            Instruction STA Absolute opcode

        0x8E ->
            Instruction STX Absolute opcode

        0x90 ->
            Instruction BCC Immediate opcode

        0x91 ->
            Instruction STA IndirectY opcode

        0x94 ->
            Instruction STY ZeroPageX opcode

        0x95 ->
            Instruction STA ZeroPageX opcode

        0x96 ->
            Instruction STX ZeroPageY opcode

        0x98 ->
            Instruction TYA Immediate opcode

        0x99 ->
            Instruction STA AbsoluteY opcode

        0x9A ->
            Instruction TXS Immediate opcode

        0x9D ->
            Instruction STA AbsoluteX opcode

        0xA0 ->
            Instruction LDY Immediate opcode

        0xA1 ->
            Instruction LDA IndirectX opcode

        0xA2 ->
            Instruction LDX Immediate opcode

        0xA4 ->
            Instruction LDY ZeroPage opcode

        0xA5 ->
            Instruction LDA ZeroPage opcode

        0xA6 ->
            Instruction LDX ZeroPage opcode

        0xA8 ->
            Instruction TAY Immediate opcode

        0xA9 ->
            Instruction LDA Immediate opcode

        0xAA ->
            Instruction TAX Immediate opcode

        0xAC ->
            Instruction LDY Absolute opcode

        0xAD ->
            Instruction LDA Absolute opcode

        0xAE ->
            Instruction LDX Absolute opcode

        0xB0 ->
            Instruction BCS Immediate opcode

        0xB1 ->
            Instruction LDA IndirectY opcode

        0xB4 ->
            Instruction LDY ZeroPageX opcode

        0xB5 ->
            Instruction LDA ZeroPageX opcode

        0xB6 ->
            Instruction LDX ZeroPageY opcode

        0xB8 ->
            Instruction CLV Immediate opcode

        0xB9 ->
            Instruction LDA AbsoluteY opcode

        0xBA ->
            Instruction TSX Immediate opcode

        0xBC ->
            Instruction LDY AbsoluteX opcode

        0xBD ->
            Instruction LDA AbsoluteX opcode

        0xBE ->
            Instruction LDX AbsoluteY opcode

        0xC0 ->
            Instruction CPY Immediate opcode

        0xC1 ->
            Instruction CMP IndirectX opcode

        0xC4 ->
            Instruction CPY ZeroPage opcode

        0xC5 ->
            Instruction CMP ZeroPage opcode

        0xC6 ->
            Instruction DEC ZeroPage opcode

        0xC8 ->
            Instruction INY Immediate opcode

        0xC9 ->
            Instruction CMP Immediate opcode

        0xCA ->
            Instruction DEX Immediate opcode

        0xCC ->
            Instruction CPY Absolute opcode

        0xCD ->
            Instruction CMP Absolute opcode

        0xCE ->
            Instruction DEC Absolute opcode

        0xD0 ->
            Instruction BNE Immediate opcode

        0xD1 ->
            Instruction CMP IndirectY opcode

        0xD5 ->
            Instruction CMP ZeroPageX opcode

        0xD6 ->
            Instruction DEC ZeroPageX opcode

        0xD8 ->
            Instruction CLD Immediate opcode

        0xD9 ->
            Instruction CMP AbsoluteY opcode

        0xDD ->
            Instruction CMP AbsoluteX opcode

        0xDE ->
            Instruction DEC AbsoluteX opcode

        0xE0 ->
            Instruction CPX Immediate opcode

        0xE1 ->
            Instruction SBC IndirectX opcode

        0xE4 ->
            Instruction CPX ZeroPage opcode

        0xE5 ->
            Instruction SBC ZeroPage opcode

        0xE6 ->
            Instruction INC ZeroPage opcode

        0xE8 ->
            Instruction INX Immediate opcode

        0xE9 ->
            Instruction SBC Immediate opcode

        0xEA ->
            Instruction NOP Immediate opcode

        0xEC ->
            Instruction CPX Absolute opcode

        0xED ->
            Instruction SBC Absolute opcode

        0xEE ->
            Instruction INC Absolute opcode

        0xF0 ->
            Instruction BEQ Immediate opcode

        0xF1 ->
            Instruction SBC IndirectY opcode

        0xF5 ->
            Instruction SBC ZeroPageX opcode

        0xF6 ->
            Instruction INC ZeroPageX opcode

        0xF8 ->
            Instruction SED Immediate opcode

        0xF9 ->
            Instruction SBC AbsoluteY opcode

        0xFD ->
            Instruction SBC AbsoluteX opcode

        0xFE ->
            Instruction INC AbsoluteX opcode

        _ ->
            Instruction ILL Implied opcode
