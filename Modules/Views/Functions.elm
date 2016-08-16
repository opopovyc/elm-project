module Modules.Views.Functions exposing(..)

import Html exposing (..)
import Html.Attributes exposing(..)
import List exposing (..)
import Chart exposing (..)

import Modules.Models.Model exposing(..)
import Modules.Updates.Msg exposing(..)
import Modules.Views.Styles exposing(styles)

--chartView: part of component's view, takes a Model, two functions (to make values and labels) and a title
--functions: List, where first element is a function that returns values (Int) from a record and second - labels (String)
chartView : Model a -> (a -> Int) -> (a -> String) -> String -> Html (Msg a)
chartView model getValue getLabel title = 
    let
        --first function is to get values
        values = map getValue model.stats
        labels = map getLabel model.stats
    in
        case model.state of  
            --model has to be a record with a field stats
            Pie -> div [] 
                [ pieView values labels title ]
            Bar -> div []
                [ node "style" [ type' "text/css" ] [text styles]
                , barView model getValue values labels
                ]
            Stat -> div [] 
                [ statView values labels]
            Empty -> div [] []

------I------
--pieView function for Pie view (using Chart module)
pieView : List Int -> List String -> String -> Html (Msg a)
pieView vals labels title =
  pie (map toFloat vals) labels
    |> Chart.title title
    |> Chart.toHtml

------II------
--barView function for Bar view
barView : Model a -> (a -> Int) -> List Int -> List String -> Html (Msg a)
barView model getValue vals labels =
  let
    --pairs is a list of (value, label) couples; from a shortStat function usingOthers field
    (percentages, pairs) = shortStat model getValue vals labels
  in
    div []
    [ div [ class "contentContainer" ]
      (List.map2 htmlBars percentages pairs)
    ]

--VALS HAVE TO BE SORTED in order to use this function!
shortStat : Model a -> (a -> Int) -> List Int -> List String -> (List Int, List (Int, String))
shortStat model function vals labels =
    let
        --count total of values
        total = totalValues model function
        --get percentage of value from total
        percent value =  (value * 100) // total 
        --filter the list of percentages of values to have the one with values more than 5% (to consider)
        filtered = filter (\z -> z > 5) (map percent vals)
        --take values, percentages of which where <= 5%, then take the sum
        others = drop (length filtered) vals |> sum
        --new list of percentages with an others field in percent
        percentages = filtered ++ [others |> percent]
        --new values list with the others field
        newVals = (take (length filtered) vals) ++ [others]
        --new labels list
        newLabels = (take (length filtered) labels) ++ ["Others"]
    in 
        (percentages, List.map2 (,) newVals newLabels)

--count total of values
totalValues : Model a -> (a -> Int) -> Int
totalValues model function = 
    map function model.stats |> List.sum

--html code for bars wrapped in a function
htmlBars :  Int -> ( Int, String ) -> Html (Msg a)
htmlBars percent (vl, lb) =
  div [ class "progressBar" ]
    [ h4 [] [ text lb ]
    , div [class "progressBarContainer" ]
      [ div [ class "progressBarValue", style [ ("width", (toString percent) ++ "%") ] ]
        []
      ]
    ]


------III------
--statView function for Stat view
--statView = table [] (matrix (newTable model))
statView : List Int -> List String -> Html (Msg a)
statView values labels = 
    let 
        --newTable: List (List String) - transform the model into 'matrix' form
        newTable = [ labels, (map toString values)]
    in 
       table [] (matrix newTable) 

--Map a 'matrix' to a list of rows of cells in a table
--Map a list of Strings to a list of table cells
matrix : List (List String) -> List (Html msg)
matrix vals = map (\row -> tr [] (map (\s -> td [] [text s]) row)) vals

--sort model by contributions (potentially by a given attributed)
sortModel : Model a -> (a -> Int) -> Model a
sortModel model function =
    { model | stats = List.reverse (List.sortBy function model.stats) }


countLabels : Model a -> Int
countLabels model =
    List.length model.stats
