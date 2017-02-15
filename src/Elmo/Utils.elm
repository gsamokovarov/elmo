module Elmo.Utils exposing (..)

import Bitwise


-- GENERAL


{-| Checks whether and address is crossing  page boundaries.
-}
pageCrossed : Int -> Int -> Bool
pageCrossed a b =
    (0xFF00 &&& a) /= (0xFF00 &&& b)


{-| Counts a boolean expression as an integral 1 if True and 0 if False. Useful
for adding or subtracting 0 or 1 based on the truthiness of an expression.
-}
count : Bool -> Int
count bool =
    if bool then
        1
    else
        0



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
