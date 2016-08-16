module Modules.Models.Model exposing (..)

import Json.Decode as Json exposing ((:=))

type alias Model a = 
    { stats : List a
    , state : State
    , url : String
    , decoder : Json.Decoder (List a)
    }

type State = Bar | Pie | Stat | Empty