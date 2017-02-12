module Elmo exposing (..)

{-| Elmo is a NES emulator written in Elm.

# Functions
@docs init
-}

import Elmo.Types exposing (System)
import Elmo.System as System


{-| Initializes a NES system.
-}
init : System
init =
    System.init
