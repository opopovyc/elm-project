module View exposing (view)

import Model exposing (Model)
import Models.MenuState exposing (..)
import Update exposing (Msg(..))
import Modules.Contributions.Stats as Stats exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html

--VIEW
view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h2 [] [text "Statistics"]
            , ul [] 
                [ button [onClick Display1] [text "Number of contributions per country"]
                , button [onClick Display2] [text "Downloads"]
                ]
            ]
        , div [] 
            [ statsView model ]
        ]

statsView : Model -> Html Msg
statsView model =
    if model.state == Stat1 then
        Html.map First (Stats.view model.stat1)
    else
        div [] []

--map the Msg messages between main and modules
--First : Stats.Msg -> Msg