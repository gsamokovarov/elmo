module Elmo.Memory exposing (init, get, set)

import Elmo.Types exposing (Memory)
import Array


init : Int -> Memory
init bytes =
    Array.repeat bytes 0


get : Int -> Memory -> Int
get address memory =
    memory
        |> Array.get address
        |> Maybe.withDefault 0


set : Int -> Int -> Memory -> Memory
set =
    Array.set
