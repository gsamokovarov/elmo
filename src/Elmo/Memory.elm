module Elmo.Memory exposing (..)

import Array exposing (Array)
import Bitwise


type alias Memory =
    Array Int


initialize : Memory
initialize =
    Array.repeat 2048 0


read : Int -> Memory -> Int
read address memory =
    Maybe.withDefault 0 <|
        Array.get address memory


read16 : Int -> Memory -> Int
read16 address memory =
    Bitwise.or
        (memory |> read (address + 1) |> Bitwise.shiftLeftBy 8)
        (memory |> read address)


read16i : Int -> Memory -> Int
read16i address memory =
    Bitwise.or
        (memory
            |> read (Bitwise.or (Bitwise.and address 0xFF00) (address + 1))
            |> Bitwise.shiftLeftBy 8
        )
        (memory |> read address)


write : Int -> Int -> Memory -> Memory
write address value memory =
    Array.set address value memory
