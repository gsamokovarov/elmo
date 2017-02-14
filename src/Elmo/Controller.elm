module Elmo.Controller exposing (..)

{-| NES input device emulation.

The gamepads are written to 0x4016 for gamepad 1 and 0x4017 for gamepad 2. This
is the format.

Read # |    1      2      3      4      5      6      7      8
-------+---------------------------------------------------------
Button |    A      B   SELECT   START   UP    DOWN   LEFT  RIGHT

See http://fms.komkon.org/EMUL8/NES.html#LABB
-}

import Elmo.Utils exposing ((<<<), (&&&), (|||), count)


{-| A snapshot of a NES input controller state.
-}
type alias Controller =
    { buttons : Int
    , i : Int
    , s : Int
    }


{-| Initializes a new controller. You may two controllers connected as
a regular devices and four with an extension pack.
-}
init : Controller
init =
    { buttons = 0
    , i = 0
    , s = 0
    }


{-| Set controller buttons from a bitmask. You can press multiple buttons at
once.

    controller
        |> Controller.press (a ||| left)
-}
press : Int -> Controller -> Controller
press buttons controller =
    { controller | buttons = buttons }


{-| Reads the buttons from controller 1 at 0x4016 or controller 2 at 0x4017.
-}
read : Controller -> (Controller -> Int)
read ({ buttons, i, s } as controller) =
    ( { controller
        | i =
            if (s &&& 1) == 1 then
                0
            else
                i + 1
      }
    , count (buttons &&& (1 <<< i) == (1 <<< i))
    )


{-| Writes the buttons to controller 1 and controller 2. Both of them should be
written to 0x4017.
-}
write : Int -> Controller -> Controller
write value ({ i, s } as controller) =
    { controller
        | s = value
        , i =
            if (value &&& 1) == 1 then
                0
            else
                i
    }



-- BUTTONS


a =
    0x01


b =
    0x02


select =
    0x04


start =
    0x08


up =
    0x10


down =
    0x20


left =
    0x40


right =
    0x80
