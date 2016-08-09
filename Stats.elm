module Stats exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
--import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
--import Json.Decode.Pipeline exposing (decode, required)
import Http
import Chart exposing (..)
import Task exposing (Task)
import List exposing (..)

type alias Line = 
    { country : String
    , contributions : Int
    }

type State = Bar | Pie | Stat | Empty

type alias Model = 
    --can add more statistics later with the record form
    { stats : List Line
    , state : State
    , url : String
    }

emptyModel: Model
emptyModel = 
  { stats = 
      [ { country = ""
        , contributions = 0
        }
      ]
  , state = Empty
  , url = "https://knowledge-gateway.org/file2.axd/e1894e44-7d5c-4035-899a-a32c23f119c6/sample_contribution_stats.json"
  }

init : ( Model, Cmd Msg )
init =
  ( --sortContrib (result emptyModel)
  emptyModel
  , getUrl emptyModel.url
  )

{-can't count members from this input?-}
countCountries : Model -> (Model -> List Line) -> Int
countCountries model stats =
    List.length (stats model)

countContributions : Model -> Int
countContributions model = 
    List.map .contributions model.stats |> List.sum

url2 : String
url2 = "https://knowledge-gateway.org/?1nf8xwvd"

url1 : String
url1 = "https://knowledge-gateway.org/file2.axd/e1894e44-7d5c-4035-899a-a32c23f119c6/sample_contribution_stats.json"

getUrl : String -> Cmd Msg
getUrl url =
    Task.perform FetchFail FetchPass (Http.get decodeUrl url)
    --Task.perform FetchFail FetchPass (Http.fromJson modelDecoder (Http.send Http.defaultSettings request))

decodeUrl : Json.Decoder (List Line)
decodeUrl =
    Json.at [ "stats" ] decodeList

decodeList : Json.Decoder (List Line)
decodeList =
    Json.list decodeLine

decodeLine : Json.Decoder Line
decodeLine =
    Json.object2 Line
        ("country" := Json.string)
        ("contributions" := Json.int)

--UPDATE

type Msg
    = FetchFail Http.Error
    | FetchPass (List Line)
    | NoOp
    | BarView
    | PieView
    | StatView

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPass response ->
          ( { model | stats = response }
          , Cmd.none
          )
        
        FetchFail err ->
          ( model
          , Cmd.none
          )

        NoOp ->
          ( model
          , Cmd.none
          )
          --List.take 10 (sortContrib (result model)).stats
        
        BarView ->
          ( { model | state = Bar }
          , Cmd.none
          )
        
        PieView ->
          ( { model | state = Pie }
          , Cmd.none
          )
        
        StatView ->
          ( { model | state = Stat }
          , Cmd.none
          )

--VIEW

{-
table : List Attribute -> List Html -> Html
represents a row of cells in a table:
tr : List Attribute -> List Html -> Html
represents a data cell in a table:
td : List Attribute -> List Html -> Html
-}
view : Model -> Html Msg
view model = 
  let
    sorted = sortContrib model
  in
    div [ id "container" ]
    [ div [ id "menu" ] 
      [ h2 [] [text "Summary" ]
--    , table [] [ tr [] (oneRow ["Members", "?"])
--    , tr [] (oneRow ["Countries and territories", (toString (countCountries model))])
--    , tr [] (oneRow ["Contributions", (toString (countContributions model))])
--    ]
      , table [] (matrix [ ["Members", "?"], ["Countries and territories", (toString (countCountries model .stats))]
      ,["Contributions", (toString (countContributions model))] ])
      , h2 [] [text "By country"]
      ]
    , div [ id "presentation" ]
      [ button [onClick BarView] [text "Bar view"]
      , button [onClick PieView] [text "Pie chart view"]
      , button [onClick StatView] [text "List statistics"]
      {-, div []
        [ node "style" [ type' "text/css" ] [text styles]
        , moreBars sorted (List.map .contributions sorted.stats) (List.map .country sorted.stats)
        ]
      , bars (List.map .contributions sorted.stats) (List.map .country sorted.stats)
      , table [] (matrix (newTable sorted))-}
      , chartView sorted
      ]
 -- table [] ((th [] (oneRow ["Country", "Contributions"])) :: (matrix (newTable (sortContrib model) )))
    ]

chartView : Model -> Html Msg
chartView model = 
  case model.state of 
    Pie -> bars (List.map .contributions model.stats) (List.map .country model.stats)
    Bar -> div []
        [ node "style" [ type' "text/css" ] [text styles]
        , moreBars model (List.map .contributions model.stats) (List.map .country model.stats)
        ]
    Stat -> table [] (matrix (newTable model))
    Empty -> div [] []


bars : List Int -> List String -> Html Msg
bars vals labels =
  pie (List.map toFloat vals) labels
    |> Chart.title "Contributions per country (in percent)"
    |> Chart.toHtml


moreBars : Model -> List Int -> List String -> Html Msg
moreBars model vals labels =
  let
    pairs : List a -> List b -> List (a,b)
    pairs lefts rights =
      List.map2 (,) lefts rights
    contrib = countContributions model
    percent vl = List.map (\z -> (z * 100) // contrib) vl 
    filtered = filter (\z -> z > 5) (percent vals)
    summed = drop (length filtered) vals |> sum |> (\z -> (z * 100) // contrib)
    combined =
        filtered ++ [summed]
    newVals = (take ((length combined) - 1) vals) ++ [summed]
    --model has to be SORTED!
    newLabels = (take ((length combined) - 1) labels) ++ ["Others"]
  in
    div []
    [ div [ class "contentContainer" ]
      (List.map2 htmlBars combined (pairs newVals newLabels))
    ]

htmlBars :  Int -> ( Int, String ) -> Html Msg
htmlBars percent (vl, lb) =
  div [ class "progressBar" ]
    [ h4 [] [ text lb ]
    , div [class "progressBarContainer" ]
      [ div [ class "progressBarValue", style [ ("width", (toString percent) ++ "%") ] ]
        []
      ]
    ]

--sort model by contributions (potentially by a given attributed)
sortContrib : Model -> Model
sortContrib model =
    { model | stats = List.reverse (List.sortBy .contributions model.stats) }

--transform the model into 'matrix' form
newTable : Model -> List (List String)
newTable model = 
    List.map (\v -> [v.country, (toString v.contributions)]) model.stats
    --model is a record stats with a list of records {country, contributions}

--Map a list of Strings to a list of table cells
oneRow : List String -> List (Html msg)
oneRow values =
    List.map (\n -> td [] [ text n ]) values

--Map a 'matrix' to a list of rows of cells in a table
matrix : List (List String) -> List (Html msg)
matrix values =
    List.map (\row -> tr [] (oneRow row)) values

--countries : Model -> List String
--countries {stats} = List.map .country stats

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none




--String and Styles    
name : String
name = """
    {
    "stats": [
        {
        "country": "Costa Rica",
        "contributions": 1
        }
    ]
    }"""

newString : String
newString = """
    {
  "stats": [
    {
      "country": "Costa Rica",
      "contributions": 1
    },
    {
      "country": "Cameroon",
      "contributions": 17
    },
    {
      "country": "Republic of the Congo",
      "contributions": 1
    },
    {
      "country": "Saudi Arabia",
      "contributions": 1
    },
    {
      "country": "Gambia",
      "contributions": 1
    },
    {
      "country": "Netherlands",
      "contributions": 1
    },
    {
      "country": "Nigeria",
      "contributions": 15
    },
    {
      "country": "Australia",
      "contributions": 1
    },
    {
      "country": "Uganda",
      "contributions": 11
    },
    {
      "country": "Burkina Faso",
      "contributions": 1
    },
    {
      "country": "Senegal",
      "contributions": 3
    },
    {
      "country": "Mozambique",
      "contributions": 2
    },
    {
      "country": "Peru",
      "contributions": 1
    },
    {
      "country": "Jordan",
      "contributions": 2
    },
    {
      "country": "Zimbabwe",
      "contributions": 1
    },
    {
      "country": "Trinidad and Tobago",
      "contributions": 1
    },
    {
      "country": "South Africa",
      "contributions": 2
    },
    {
      "country": "Mauritania",
      "contributions": 1
    },
    {
      "country": "Tunisia",
      "contributions": 2
    },
    {
      "country": "Croatia",
      "contributions": 1
    },
    {
      "country": "India",
      "contributions": 27
    },
    {
      "country": "Philippines",
      "contributions": 2
    },
    {
      "country": "Sudan",
      "contributions": 4
    },
    {
      "country": "Tanzania",
      "contributions": 1
    },
    {
      "country": "Madagascar",
      "contributions": 1
    },
    {
      "country": "Iraq",
      "contributions": 5
    },
    {
      "country": "France",
      "contributions": 2
    },
    {
      "country": "Nepal",
      "contributions": 1
    },
    {
      "country": "Denmark",
      "contributions": 1
    },
    {
      "country": "Bangladesh",
      "contributions": 1
    },
    {
      "country": "Swaziland",
      "contributions": 2
    },
    {
      "country": "Egypt",
      "contributions": 5
    },
    {
      "country": "Zambia",
      "contributions": 2
    },
    {
      "country": "United States",
      "contributions": 1156
    },
    {
      "country": "Benin",
      "contributions": 3
    },
    {
      "country": "Afghanistan",
      "contributions": 1
    },
    {
      "country": "Ghana",
      "contributions": 8
    },
    {
      "country": "Ethiopia",
      "contributions": 18
    },
    {
      "country": "Democratic Republic of the Congo",
      "contributions": 2
    },
    {
      "country": "Canada",
      "contributions": 5
    },
    {
      "country": "Kenya",
      "contributions": 10
    },
    {
      "country": "United Kingdom",
      "contributions": 25
    },
    {
      "country": "Pakistan",
      "contributions": 5
    },
    {
      "country": "Yemen",
      "contributions": 1
    },
    {
      "country": "Switzerland",
      "contributions": 20
    },
    {
      "country": "Indonesia",
      "contributions": 7
    },
    {
      "country": "Belgium",
      "contributions": 426
    },
    {
      "country": "Mali",
      "contributions": 1
    }
  ]
}"""

styles : String
styles = 
    """#container { display: flex; }
      
      .contentContainer {
        background: #efefef;
        padding: 20px;
        max-width: 350px;
        min-width: 150px;
        margin: 15vh auto;
        border-radius: 10px;
        border: solid 5px #dbdbdb;
      }

      .progressBar {
        margin-bottom: 26px;
        margin-bottom: 1.66em;
      }

      .progressBar h4 {
        font-size: 21px;
        font-size: 1.33em;
        text-transform: none;
        font-family: Arial, Helvetica, sans-serif;
        font-weight: bold;
        margin-bottom: 7px;
        margin-bottom: .33em;
      }

      .progressBarContainer {
        width: 100%;
        max-width: 350px;
        height: 26px;
        height: 1.66em;
        background: #e6eae3;
        background: rgba(8,102,220,.2);
        overflow: hidden;
        border-radius: 5px;
      }

      .progressBarValue {
        height: 1.66em;
        float: left;
        background: #0866dc;
        background: rgba(8,102,220,.75);
      }
    """

