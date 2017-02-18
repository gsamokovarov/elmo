module Elmo.Ppu.Palette exposing (Palette, default, nesClassic)

{-| A PPU color palette. It tries to decode a NTSC video signal and map its
colors to RGB using one of the palettes defined in this module.

See https://wiki.nesdev.com/w/index.php/PPU_palettes
-}

import Elmo.Types exposing (Memory)
import Array


{-| The default palette is the PVM Style (FBX) palette from
http://www.firebrandx.com/nespalette.html. I have chosen this palette
specifically, because it approximate the color of a Sony PVM CRT screen and
match it to a digital display, since most of use use those nowadays.
-}
default : Memory
default =
    Array.fromList
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


{-| This palette is the one found in the NES Classic mini console. It looks
pretty good as well.
-}
nesClassic : Memory
nesClassic =
    Array.fromList
        [ 0x0060615F
        , 0x83
        , 0x001D0195
        , 0x00340875
        , 0x0051055E
        , 0x0056000F
        , 0x004C0700
        , 0x00372308
        , 0x00203A0B
        , 0x000F4B0E
        , 0x00194C16
        , 0x0002421E
        , 0x00023154
        , 0x00
        , 0x00
        , 0x00
        , 0x00A9AAA8
        , 0x00104BBF
        , 0x004712D8
        , 0x006300CA
        , 0x008800A9
        , 0x00930B46
        , 0x008A2D04
        , 0x006F5206
        , 0x005C7114
        , 0x001B8D12
        , 0x00199509
        , 0x00178448
        , 0x00206B8E
        , 0x00
        , 0x00
        , 0x00
        , 0x00FBFBFB
        , 0x006699F8
        , 0x008974F9
        , 0x00AB58F8
        , 0x00D557EF
        , 0x00DE5FA9
        , 0x00DC7F59
        , 0x00C7A224
        , 0x00A7BE03
        , 0x0075D703
        , 0x0060E34F
        , 0x003CD68D
        , 0x0056C9CC
        , 0x00414240
        , 0x00
        , 0x00
        , 0x00FBFBFB
        , 0x00BED4FA
        , 0x00C9C7F9
        , 0x00D7BEFA
        , 0x00E8B8F9
        , 0x00F5BAE5
        , 0x00F3CAC2
        , 0x00DFCDA7
        , 0x00D9E09C
        , 0x00C9EB9E
        , 0x00C0EDB8
        , 0x00B5F4C7
        , 0x00B9EAE9
        , 0x00ABABAB
        , 0x00
        , 0x00
        ]
