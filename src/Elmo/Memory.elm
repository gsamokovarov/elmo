module Elmo.Memory exposing (..)

import Array exposing (Array)


type alias Memory =
    Array Int


initialize : Memory
initialize =
    Array.repeat 2048 0


read : Int -> Memory -> Int
read address memory =
    Maybe.withDefault 0 <|
        Array.get address memory


write : Int -> Int -> Memory -> Memory
write address value memory =
    Array.set address value memory
