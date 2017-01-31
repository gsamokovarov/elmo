module Elmo.Opcode exposing (..)

{-| Opcode dispatching code for the 2A03 CPU.

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


type Label
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


type alias Opcode =
    { label : Label
    , mode : AddressingMode
    , opcode : Int
    , cycles : Int
    , pageCycles : Int
    , branchPageCycles : Int
    }


dispatch : Int -> Opcode
dispatch opcode =
    case opcode of
        0x00 ->
            Opcode BRK Implied opcode 7 0 0

        0x01 ->
            Opcode ORA IndirectX opcode 6 0 0

        0x05 ->
            Opcode ORA ZeroPage opcode 3 0 0

        0x06 ->
            Opcode ASL ZeroPage opcode 5 0 0

        0x08 ->
            Opcode PHP Implied opcode 3 0 0

        0x09 ->
            Opcode ORA Immediate opcode 2 0 0

        0x0A ->
            Opcode ASL Accumulator opcode 2 0 0

        0x0D ->
            Opcode ORA Absolute opcode 4 0 0

        0x0E ->
            Opcode ASL Absolute opcode 6 0 0

        0x10 ->
            Opcode BPL Immediate opcode 2 0 2

        0x11 ->
            Opcode ORA IndirectY opcode 5 1 0

        0x15 ->
            Opcode ORA ZeroPageX opcode 4 0 0

        0x16 ->
            Opcode ASL ZeroPageX opcode 6 0 0

        0x18 ->
            Opcode CLC Immediate opcode 2 0 0

        0x19 ->
            Opcode ORA AbsoluteY opcode 4 1 0

        0x1D ->
            Opcode ORA AbsoluteX opcode 4 1 0

        0x1E ->
            Opcode ASL AbsoluteX opcode 7 0 0

        0x20 ->
            Opcode JSR Absolute opcode 6 0 0

        0x21 ->
            Opcode AND IndirectX opcode 6 0 0

        0x24 ->
            Opcode BIT ZeroPage opcode 3 0 0

        0x25 ->
            Opcode AND ZeroPage opcode 3 0 0

        0x26 ->
            Opcode ROL ZeroPage opcode 5 0 0

        0x28 ->
            Opcode PLP Implied opcode 4 0 0

        0x29 ->
            Opcode AND Immediate opcode 2 0 0

        0x2A ->
            Opcode ROL Accumulator opcode 2 0 0

        0x2C ->
            Opcode BIT Absolute opcode 4 0 0

        0x2D ->
            Opcode AND Absolute opcode 4 0 0

        0x2E ->
            Opcode ROL Absolute opcode 6 0 0

        0x30 ->
            Opcode BMI Immediate opcode 2 0 2

        0x31 ->
            Opcode AND IndirectY opcode 5 1 0

        0x35 ->
            Opcode AND ZeroPageX opcode 4 0 0

        0x36 ->
            Opcode ROL ZeroPageX opcode 6 0 0

        0x38 ->
            Opcode SEC Implied opcode 2 0 0

        0x39 ->
            Opcode AND AbsoluteY opcode 4 1 0

        0x3D ->
            Opcode AND AbsoluteX opcode 4 1 0

        0x3E ->
            Opcode ROL AbsoluteX opcode 7 0 0

        0x40 ->
            Opcode RTI Implied opcode 6 0 0

        0x41 ->
            Opcode EOR IndirectX opcode 6 0 0

        0x45 ->
            Opcode EOR ZeroPage opcode 3 0 0

        0x46 ->
            Opcode LSR ZeroPage opcode 5 0 0

        0x48 ->
            Opcode PHA Implied opcode 3 0 0

        0x49 ->
            Opcode EOR Immediate opcode 2 0 0

        0x4A ->
            Opcode LSR Accumulator opcode 2 0 0

        0x4C ->
            Opcode JMP Absolute opcode 3 0 0

        0x4D ->
            Opcode EOR Absolute opcode 4 0 0

        0x4E ->
            Opcode LSR Absolute opcode 6 0 0

        0x50 ->
            Opcode BVC Relative opcode 2 0 2

        0x51 ->
            Opcode EOR IndirectY opcode 5 1 0

        0x55 ->
            Opcode EOR ZeroPageX opcode 4 0 0

        0x56 ->
            Opcode LSR ZeroPageX opcode 6 0 0

        0x58 ->
            Opcode CLI Implied opcode 2 0 0

        0x59 ->
            Opcode EOR AbsoluteY opcode 4 1 0

        0x5D ->
            Opcode EOR AbsoluteX opcode 4 1 0

        0x5E ->
            Opcode LSR AbsoluteX opcode 7 0 0

        0x60 ->
            Opcode RTS Implied opcode 5 0 0

        0x61 ->
            Opcode ADC IndirectX opcode 6 0 0

        0x65 ->
            Opcode ADC ZeroPage opcode 3 0 0

        0x66 ->
            Opcode ROR ZeroPage opcode 5 0 0

        0x68 ->
            Opcode PLA Implied opcode 4 0 0

        0x69 ->
            Opcode ADC Immediate opcode 2 0 0

        0x6A ->
            Opcode ROR Accumulator opcode 2 0 0

        0x6C ->
            Opcode JMP Indirect opcode 5 0 0

        0x6D ->
            Opcode ADC Absolute opcode 4 0 0

        0x6E ->
            Opcode ROR Absolute opcode 6 0 0

        0x70 ->
            Opcode BVS Relative opcode 2 0 2

        0x71 ->
            Opcode ADC IndirectY opcode 5 1 0

        0x75 ->
            Opcode ADC ZeroPageX opcode 4 0 0

        0x76 ->
            Opcode ROR ZeroPageX opcode 6 0 0

        0x78 ->
            Opcode SEI Implied opcode 2 0 0

        0x79 ->
            Opcode ADC AbsoluteY opcode 4 1 0

        0x7D ->
            Opcode ADC AbsoluteX opcode 4 1 0

        0x7E ->
            Opcode ROR AbsoluteX opcode 7 0 0

        0x81 ->
            Opcode STA IndirectX opcode 6 0 0

        0x84 ->
            Opcode STY ZeroPage opcode 3 0 0

        0x85 ->
            Opcode STA ZeroPage opcode 3 0 0

        0x86 ->
            Opcode STX ZeroPage opcode 3 0 0

        0x88 ->
            Opcode DEY Implied opcode 2 0 0

        0x8A ->
            Opcode TXA Immediate opcode 2 0 0

        0x8C ->
            Opcode STY Absolute opcode 4 0 0

        0x8D ->
            Opcode STA Absolute opcode 4 0 0

        0x8E ->
            Opcode STX Absolute opcode 4 0 0

        0x90 ->
            Opcode BCC Relative opcode 2 0 2

        0x91 ->
            Opcode STA IndirectY opcode 6 0 0

        0x94 ->
            Opcode STY ZeroPageX opcode 4 0 0

        0x95 ->
            Opcode STA ZeroPageX opcode 4 0 0

        0x96 ->
            Opcode STX ZeroPageY opcode 4 0 0

        0x98 ->
            Opcode TYA Implied opcode 2 0 0

        0x99 ->
            Opcode STA AbsoluteY opcode 5 0 0

        0x9A ->
            Opcode TXS Implied opcode 2 0 0

        0x9D ->
            Opcode STA AbsoluteX opcode 5 0 0

        0xA0 ->
            Opcode LDY Immediate opcode 2 0 0

        0xA1 ->
            Opcode LDA IndirectX opcode 6 0 0

        0xA2 ->
            Opcode LDX Immediate opcode 2 0 0

        0xA4 ->
            Opcode LDY ZeroPage opcode 3 0 0

        0xA5 ->
            Opcode LDA ZeroPage opcode 3 0 0

        0xA6 ->
            Opcode LDX ZeroPage opcode 3 0 0

        0xA8 ->
            Opcode TAY Implied opcode 2 0 0

        0xA9 ->
            Opcode LDA Immediate opcode 2 0 0

        0xAA ->
            Opcode TAX Implied opcode 2 0 0

        0xAC ->
            Opcode LDY Absolute opcode 4 0 0

        0xAD ->
            Opcode LDA Absolute opcode 4 0 0

        0xAE ->
            Opcode LDX Absolute opcode 4 0 0

        0xB0 ->
            Opcode BCS Relative opcode 2 0 2

        0xB1 ->
            Opcode LDA IndirectY opcode 5 1 0

        0xB4 ->
            Opcode LDY ZeroPageX opcode 4 0 0

        0xB5 ->
            Opcode LDA ZeroPageX opcode 4 0 0

        0xB6 ->
            Opcode LDX ZeroPageY opcode 4 0 0

        0xB8 ->
            Opcode CLV Implied opcode 2 0 0

        0xB9 ->
            Opcode LDA AbsoluteY opcode 4 1 0

        0xBA ->
            Opcode TSX Implied opcode 2 0 0

        0xBC ->
            Opcode LDY AbsoluteX opcode 4 1 0

        0xBD ->
            Opcode LDA AbsoluteX opcode 4 1 0

        0xBE ->
            Opcode LDX AbsoluteY opcode 4 1 0

        0xC0 ->
            Opcode CPY Immediate opcode 2 0 0

        0xC1 ->
            Opcode CMP IndirectX opcode 6 0 0

        0xC4 ->
            Opcode CPY ZeroPage opcode 3 0 0

        0xC5 ->
            Opcode CMP ZeroPage opcode 3 0 0

        0xC6 ->
            Opcode DEC ZeroPage opcode 5 0 0

        0xC8 ->
            Opcode INY Implied opcode 2 0 0

        0xC9 ->
            Opcode CMP Immediate opcode 2 0 0

        0xCA ->
            Opcode DEX Implied opcode 2 0 0

        0xCC ->
            Opcode CPY Absolute opcode 4 0 0

        0xCD ->
            Opcode CMP Absolute opcode 4 0 0

        0xCE ->
            Opcode DEC Absolute opcode 6 0 0

        0xD0 ->
            Opcode BNE Immediate opcode 2 0 2

        0xD1 ->
            Opcode CMP IndirectY opcode 5 1 0

        0xD5 ->
            Opcode CMP ZeroPageX opcode 4 0 0

        0xD6 ->
            Opcode DEC ZeroPageX opcode 6 0 0

        0xD8 ->
            Opcode CLD Immediate opcode 2 0 0

        0xD9 ->
            Opcode CMP AbsoluteY opcode 4 1 0

        0xDD ->
            Opcode CMP AbsoluteX opcode 4 1 0

        0xDE ->
            Opcode DEC AbsoluteX opcode 7 0 0

        0xE0 ->
            Opcode CPX Immediate opcode 2 0 0

        0xE1 ->
            Opcode SBC IndirectX opcode 6 0 0

        0xE4 ->
            Opcode CPX ZeroPage opcode 3 0 0

        0xE5 ->
            Opcode SBC ZeroPage opcode 3 0 0

        0xE6 ->
            Opcode INC ZeroPage opcode 5 0 0

        0xE8 ->
            Opcode INX Implied opcode 2 0 0

        0xE9 ->
            Opcode SBC Immediate opcode 2 0 0

        0xEA ->
            Opcode NOP Immediate opcode 2 0 0

        0xEC ->
            Opcode CPX Absolute opcode 4 0 0

        0xED ->
            Opcode SBC Absolute opcode 4 0 0

        0xEE ->
            Opcode INC Absolute opcode 6 0 0

        0xF0 ->
            Opcode BEQ Relative opcode 2 0 2

        0xF1 ->
            Opcode SBC IndirectY opcode 5 1 0

        0xF5 ->
            Opcode SBC ZeroPageX opcode 4 0 0

        0xF6 ->
            Opcode INC ZeroPageX opcode 6 0 0

        0xF8 ->
            Opcode SED Implied opcode 2 0 0

        0xF9 ->
            Opcode SBC AbsoluteY opcode 4 1 0

        0xFD ->
            Opcode SBC AbsoluteX opcode 4 1 0

        0xFE ->
            Opcode INC AbsoluteX opcode 7 0 0

        _ ->
            Opcode ILL Implied opcode 0 0 0
