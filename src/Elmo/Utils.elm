module Elmo.Utils exposing (..)

import Elmo.Math exposing ((&&&))


pageCrossed : Int -> Int -> Bool
pageCrossed a b =
    (0xFF00 &&& a) /= (0xFF00 &&& b)


count : Bool -> Int
count bool =
    if bool then
        1
    else
        0
