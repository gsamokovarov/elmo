module Elmo.Flags exposing (..)

{-| This module provide utilities for working with the status register.

    The register consists of eight flags. Bits of this register
    are altered depending on the result of arithmetic and logical operations.
    These bits are described below:

    7   6   5   4   3   2   1   0
    S   V       B   D   I   Z   C

    See docs/6502.txt for more details on the role of each of the flags above.
-}

import Elmo.Utils exposing ((|||), (&&&), (^^^))


init : Int
init =
    {- In the initial state, the fifth byte, which is unused, is always up.
       0b0010_0000
    -}
    32



-- FLAGS


carry : Int
carry =
    1


setCarry : Bool -> Int -> Int
setCarry =
    setBit carry


zero : Int
zero =
    2


setZero : Int -> Int -> Int
setZero value =
    setBit zero (value == 0)


interrupt : Int
interrupt =
    4


setInterrupt : Bool -> Int -> Int
setInterrupt =
    setBit interrupt


decimal : Int
decimal =
    8


setDecimal : Bool -> Int -> Int
setDecimal =
    setBit decimal


brk : Int
brk =
    16


setBrk : Bool -> Int -> Int
setBrk =
    setBit brk


overflow : Int
overflow =
    64


setOverflow : Bool -> Int -> Int
setOverflow =
    setBit overflow


sign : Int
sign =
    128


setSign : Int -> Int -> Int
setSign value =
    setBit sign ((value &&& sign) /= 0)



-- INTERNAL


setBit : Int -> Bool -> Int -> Int
setBit bit trigger flags =
    if trigger then
        flags ||| bit
    else
        flags &&& (0xFF ^^^ bit)
