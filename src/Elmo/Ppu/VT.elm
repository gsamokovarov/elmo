module Elmo.Ppu.VT
    exposing
        ( coarseX
        , getCoarseX
        , setCoarseX
        , coarseY
        , getCoarseY
        , setCoarseY
        , nametable
        , getNametable
        , setNametable
        , fineY
        , getFineY
        , setFineY
        )

{-| V and T are 15 bit registers composed this way during rendering.

    yyy NN YYYYY XXXXX
    ||| || ||||| +++++-- coarse X scroll
    ||| || +++++-------- coarse Y scroll
    ||| ++-------------- nametable select
    +++----------------- fine Y scroll

    http://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_registers
-}

import Elmo.Utils exposing ((|||), (&&&), (<<<))


-- COARSE X


coarseX : Int
coarseX =
    0x1F


getCoarseX : Int -> Int
getCoarseX vt =
    vt &&& coarseX


setCoarseX : Int -> Int -> Int
setCoarseX value vt =
    value &&& coarseX



-- COARSE Y


coarseY : Int
coarseY =
    0x03E0


getCoarseY : Int -> Int
getCoarseY vt =
    vt &&& coarseY


setCoarseY : Int -> Int -> Int
setCoarseY value vt =
    ((value <<< 5) &&& coarseY) ||| vt



-- NAMETABLE


nametable : Int
nametable =
    0x0C00


getNametable : Int -> Int
getNametable vt =
    vt &&& nametable


setNametable : Int -> Int -> Int
setNametable value vt =
    ((value <<< 10) &&& nametable) ||| vt



-- FINE Y


fineY : Int
fineY =
    0x7000


getFineY : Int -> Int
getFineY vt =
    vt &&& fineY


setFineY : Int -> Int -> Int
setFineY value vt =
    ((value <<< 12) &&& fineY) ||| vt
