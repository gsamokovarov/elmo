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


{-| 8-bit wrap around addition.
-}
(|+|) : Int -> Int -> Int
(|+|) a b =
    Bitwise.and 0xFF (a + b)
infixl 6 |+|


{-| 8-bit wrap around addition.
-}
(|-|) : Int -> Int -> Int
(|-|) a b =
    Bitwise.and 0xFF (a - b)
infixl 6 |-|


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
