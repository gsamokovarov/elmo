module Elmo.Controller exposing (..)

{-| NES input device emulation.

The gamepads are written to 0x4016 for gamepad 1 and 0x4017 for gamepad 2. This
is the format.

Read # |    1      2      3      4      5      6      7      8
-------+---------------------------------------------------------
Button |    A      B   SELECT   START   UP    DOWN   LEFT  RIGHT

See http://fms.komkon.org/EMUL8/NES.html#LABB
-}


{-| A snapshot of a NES input controller state.
-}
type alias Controller =
    { buttons : Int
    , s : Int
    }


{-| Initializes a new controller. You may two controllers connected as
a regular devices and four with an extension pack.
-}
init : Controller
init =
    { buttons = 0
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
