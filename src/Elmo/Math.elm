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
(<<<) a b =
    Bitwise.shiftLeftBy b a


{-| Shift the number a with b bits to the left.
-}
(>>>) : Int -> Int -> Int
(>>>) a b =
    Bitwise.shiftRightBy b a



-- PRIORITIES


infixl 4 &&&


infixl 4 |||


infixl 4 ^^^


infixl 5 <<<


infixl 5 >>>
