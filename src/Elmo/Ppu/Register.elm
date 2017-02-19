module Elmo.Ppu.Register exposing (read, write)

{-| PPU registers memory mapped to the CPU at 0x2000 to 0x2007. Both reading and wr
-}

import Elmo.Ppu.Status as Status
import Elmo.Ppu.Control as Control
import Elmo.Ppu.VT as VT
import Elmo.Types exposing (Ppu)
import Elmo.Utils exposing ((&&&), bitSet)


read : Int -> Ppu -> ( Ppu, Int )
read address ppu =
    case address of
        2 ->
            ( { ppu
                | w = False
                , ppustatus = ppu.ppustatus |> Status.setVerticalBlank False
              }
            , ppu.ppustatus &&& 0x1F
            )

        4 ->
            ( ppu
            , ppu.spriteMemory |> Memory.get ppu.oamaddr
            )

        7 ->
            ( { ppu
                | v =
                    if ppu.ppustatus |> bitSet Status.increment then
                        ppu.v + 32
                    else
                        ppu.v + 1
              }
            , 2
            )

        _ ->
            ( ppu, 0 )
