module Init exposing (initialModel)

--import State, Stats and Msg
import Models.MenuState exposing (..)
import Model exposing (Model)
import Modules.Contributions.Stats as Stats exposing (init, Model)
import Update exposing (..)


initialModel : (Model.Model, Cmd Msg)
initialModel =
    let
        (first, firstCmd) = Stats.init
    in
        (
            { state = Empty
            , stat1 = first
            }
        , Cmd.map FirstInit firstCmd
        )