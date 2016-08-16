module View exposing (view)

import Model exposing (Model)
import Models.MenuState exposing (..)
import Update exposing (Msg(..))
import Modules.Views.View as ModuleView exposing(view)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html

--VIEW
view : Model -> Html Update.Msg
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

statsView : Model -> Html Update.Msg
statsView model =
    if model.state == Stat1 then
        Html.map First (ModuleView.view model.stat1 ["Contributions per country (in percent)"] .contributions .country)
    else if model.state == Stat2 then
        Html.map Second (ModuleView.view model.stat2 ["Downloads"] .downloads .title)
    else
        div [] []

--map the Msg messages between main and modules
