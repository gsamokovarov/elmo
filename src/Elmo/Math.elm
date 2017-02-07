module Elmo.Math exposing (..)

import Bitwise


-- OPERATORS


{-| Bitwise logical and operation.
-}
(&&&) : Int -> Int -> Int
(&&&) =
    Bitwise.and


{-| Bitwise logical or operation.
-}
(|||) : Int -> Int -> Int
(|||) =
    Bitwise.or


{-| Bitwise logical or operation.
-}
(^^^) : Int -> Int -> Int
(^^^) =
    Bitwise.xor


{-| Shift the number a with b bits to the left.
-}
(<<<) : Int -> Int -> Int
(<<<) =
    flip Bitwise.shiftLeftBy


{-| Shift the number a with b bits to the left.
-}
(>>>) : Int -> Int -> Int
(>>>) =
    flip Bitwise.shiftRightBy



-- PRIORITIES


infixl 4 &&&


infixl 4 |||


infixl 4 ^^^


infixl 5 <<<


infixl 5 >>>
