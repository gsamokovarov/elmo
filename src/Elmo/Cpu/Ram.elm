module Elmo.Cpu.Ram exposing (..)

import Elmo.Utils exposing ((|||), (&&&))
import Elmo.Types exposing (Memory)
import Elmo.Memory as Memory
import Array exposing (Array)
import Bitwise


read : Int -> Memory -> Int
read address ram =
    if address < 0x2000 then
        ram |> Memory.get (address % 0x0800)
    else
        0


read16 : Int -> Memory -> Int
read16 address ram =
    Bitwise.or
        (ram |> read (address + 1) |> Bitwise.shiftLeftBy 8)
        (ram |> read address)


read16i : Int -> Memory -> Int
read16i address ram =
    Bitwise.or
        (ram
            |> read ((address &&& 0xFF00) ||| (address + 1))
            |> Bitwise.shiftLeftBy 8
        )
        (ram |> read address)


write : Int -> Int -> Memory -> Memory
write =
    Memory.set
