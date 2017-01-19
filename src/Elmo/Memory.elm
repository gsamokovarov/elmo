module Elm.Memory exposing (..)

import Array


type alias Memory =
    Array Int


initialize : Memory
initialize =
    Array.repeat 2048 0


read : Int -> Memory
read address memory =
    Array.get address memory


write : Int -> Int -> Memory
write address value memory =
    Array.set address value memory
