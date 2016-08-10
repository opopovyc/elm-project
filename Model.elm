module Model exposing (Model)

import Models.MenuState exposing (..)
import Modules.Contributions.Stats as Stats exposing (init, Model)

type alias Model =
    { state : State 
    , stat1 : Stats.Model
    }