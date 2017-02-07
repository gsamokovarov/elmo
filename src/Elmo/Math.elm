module Elmo.Math exposing (..)

import Bitwise


{-| Bitwise logical and operation.
-}
(&&&) : Int -> Int -> Int
(&&&) =
    Bitwise.and
infixl 4 &&&


{-| Bitwise logical or operation.
-}
(|||) : Int -> Int -> Int
(|||) =
    Bitwise.or
infixl 4 |||


{-| Bitwise logical or operation.
-}
(^^^) : Int -> Int -> Int
(^^^) =
    Bitwise.xor
infixl 4 ^^^


{-| Shift the number a with b bits to the left.
-}
(<<<) : Int -> Int -> Int
(<<<) a b =
    Bitwise.shiftLeftBy b a
infixl 5 <<<


{-| Shift the number a with b bits to the left.
-}
(>>>) : Int -> Int -> Int
(>>>) a b =
    Bitwise.shiftRightBy b a
infixl 5 >>>
