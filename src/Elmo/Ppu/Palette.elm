module Elmo.Ppu.Palette exposing (Palette, default)

{-| A PPU color palette. It tries to decode a NTSC video signal and map its
colors to RGB using one of the palettes defined in this module.

See https://wiki.nesdev.com/w/index.php/PPU_palettes
-}


{-| Palette is an array of 3-byte integers. The 3 bytes represent RGB color
values.
-}
type alias Palette =
    Array Int


{-| The default palette is the PVM Style (FBX) palette from
http://www.firebrandx.com/nespalette.html. I have chosen this palette
specifically, because it approximate the color of a Sony PVM CRT screen and
match it to a digital display, since most of use use those nowadays.
-}
default : Palette
default =
    [ 0x00696964
    , 0x1774
    , 0x0028007D
    , 0x003E006D
    , 0x00560057
    , 0x005E0013
    , 0x00531A00
    , 0x003B2400
    , 0x002A3000
    , 0x00143A00
    , 0x3F00
    , 0x3B1E
    , 0x3050
    , 0x00
    , 0x00
    , 0x00
    , 0x00B9B9B4
    , 0x001453B9
    , 0x004D2CDA
    , 0x007A1EC8
    , 0x0098189C
    , 0x009D2344
    , 0x00A03E00
    , 0x008D5500
    , 0x00656D00
    , 0x002C7900
    , 0x8100
    , 0x7D42
    , 0x788A
    , 0x00
    , 0x00
    , 0x00
    , 0x00FFFFFF
    , 0x0069A8FF
    , 0x009A96FF
    , 0x00C28AFA
    , 0x00EA7DFA
    , 0x00F387B4
    , 0x00F1986C
    , 0x00E6B327
    , 0x00D7C805
    , 0x0090DF07
    , 0x0064E53C
    , 0x0045E27D
    , 0x0048D5D9
    , 0x004B4B46
    , 0x00
    , 0x00
    , 0x00FFFFFF
    , 0x00D2EAFF
    , 0x00E2E2FF
    , 0x00F2D8FF
    , 0x00F8D2FF
    , 0x00F8D9EA
    , 0x00FADEB9
    , 0x00F9E89B
    , 0x00F3F28C
    , 0x00D3FA91
    , 0x00B8FCA8
    , 0x00AEFACA
    , 0x00CAF3F3
    , 0x00BEBEB9
    , 0x00
    , 0x00
    ]
