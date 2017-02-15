module Elmo.Memory exposing (..)

import Elmo.Utils exposing ((|||), (&&&))
import Array exposing (Array)
import Bitwise


init : Memory
init =
    Array.repeat 2048 0


read : Int -> Memory -> Int
read address memory =
    if address < 0x2000 then
        memory
            |> Array.get (address % 2048)
            |> Maybe.withDefault 0
    else
        0


read16 : Int -> Memory -> Int
read16 address memory =
    Bitwise.or
        (memory |> read (address + 1) |> Bitwise.shiftLeftBy 8)
        (memory |> read address)


read16i : Int -> Memory -> Int
read16i address memory =
    Bitwise.or
        (memory
            |> read ((address &&& 0xFF00) ||| (address + 1))
            |> Bitwise.shiftLeftBy 8
        )
        (memory |> read address)


write : Int -> Int -> Memory -> Memory
write address value memory =
    Array.set address value memory
