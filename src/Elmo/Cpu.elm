module Elm.Cpu exposing (..)

{-| The NES Central Processing Unit was a variant of the 8-bit 6502 processor.

   It's called 2A03 and it's based on the 6502. The chip knows how to handle
   sound, having a pAPU (psuedo-Audio Processing Unit). However, it lacks Binary
   Coded Decimal mode.
-}


type Interrupt
    = None
    | NMI
    | IRQ


type AddressingMode
    = Absolute
    | AbsuluteX
    | AbsoluteY
    | Accumulator
    | Immediate
    | Implied
    | IndexedIndirect
    | Indirect
    | IndirectIndexed
    | Relative
    | ZeroPage
    | ZeroPageX
    | ZeroPageY


type Instruction
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
    | KIL
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


type alias Cpu =
    { pc : Int
    , sp : Int
    , a : Int
    , x : Int
    , y : Int
    , p : Int
    , interrupt : Interrupt
    , mode : AddressingMode
    , stall : Int
    , cycles : Int
    }


step : Cpu -> Cpu
step cpu =
    if cpu.stall > 0 then
        { cpu | stall = stall - 1 }
    else
       -- TODO(genadi): Handle interrupts here.

      inst = read cpu

