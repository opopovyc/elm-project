module Modules.Views.View exposing (..)

import Modules.Views.Functions exposing(..)
import Modules.Updates.Msg exposing(..)
import Modules.Models.Model exposing(..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)

view : Model a -> List String -> (a -> Int) -> (a -> String) -> Html (Msg a)
view model stats getValue getLabel =
  let
    sorted = sortModel model getValue
    total = toString (totalValues model getValue)
    count = toString (countLabels model)
    --to change
    title =
        case List.head stats of
            Just s ->
                s
            Nothing ->
                ""
  in
    div [ id "container" ]
        [ div [ id "menu" ]
            [ h2 [] [text "Summary" ]
            , table [] (matrix
                [ ["Countries and territories", count]
                , ["Contributions", total]
                ])
            , h2 [] [text "By country"]
            ]
        , div [] [text (toString count)]
        , div [] [text (toString total)]
        , div [ id "presentation" ]
            [ button [onClick BarView] [text "Bar view"]
            , button [onClick PieView] [text "Pie chart view"]
            , button [onClick StatView] [text "List statistics"]
            , chartView sorted getValue getLabel title
            ]
        ]