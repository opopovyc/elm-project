import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Json.Decode.Pipeline exposing (decode, required)
import Http
import Chart exposing (..)
import Task exposing (Task)

--?


main = Html.program
        { init = init --model = initialModel
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

type alias Line = 
    { country : String
    --should contributions be modelled as Int?
    , contributions : Int
    }

type alias Model = 
    --can add more statistics later with the record form
    { stats : List Line
    }

emptyModel: Model
emptyModel = 
  { stats = 
      [ { country = ""
        , contributions = 0
        }
      ]
  }

init : ( Model, Cmd Msg )
init =
  ( --sortContrib (result emptyModel)
  emptyModel
  , Cmd.none
  )

{-
decodeModel : Json.Decode.Value -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeValue modelDecoder modelJson
-}

lineDecoder : Json.Decoder Line
lineDecoder = 
    Json.Decode.Pipeline.decode Line
        |> Json.Decode.Pipeline.required "country" Json.string
        |> Json.Decode.Pipeline.required "contributions" Json.int

modelDecoder : Json.Decoder Model
modelDecoder = 
    Json.Decode.Pipeline.decode Model
        |> Json.Decode.Pipeline.required "stats" (Json.list lineDecoder)

{- String for now, normally some other type of Json values 
Json.Decode.decodeString modelDecoder-}
decodeModel : String -> Result String Model
decodeModel modelJson =
    Json.decodeString modelDecoder modelJson

--use of the Json decoder on Json value (newString) 
result : Model -> Model
result model = 
    Result.withDefault model (decodeModel newString)

{-can't count members from this input?-}
countCountries : Model -> Int
countCountries model =
    List.length model.stats

countContributions : Model -> Int
countContributions model = 
    List.map .contributions model.stats |> List.sum

getUrl : Cmd Msg
getUrl =
  let
    url = "https://knowledge-gateway.org/file2.axd/e1894e44-7d5c-4035-899a-a32c23f119c6/sample_contribution_stats.json" 
    request = 
      { verb = "GET"
      , headers = 
        [ ("Origin", "http://localhost:8000")
        , ("Access-Control-Request-Headers", "X-Custom-Header")
        ]
      , url = url
      , body = Http.empty
    }
  in
    --Task.perform FetchFail FetchPass (Http.get Json.string url)
    Task.perform FetchFail FetchPass (Http.fromJson modelDecoder (Http.send Http.defaultSettings request))

decodeUrl =
    Json.at [ "stats" ] decodeList


decodeList =
    Json.list decodeLine

decodeLine =
    Json.object2 Line
        ("country" := Json.string)
        ("contributions" := Json.int)

type Msg
    = Display
    | FetchFail Http.Error
    | FetchPass Model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Display ->
          ( model
          , getUrl
          )
        
        FetchPass response ->
          ( response
          , Cmd.none
          )
        
        FetchFail err ->
          ( model
          , Cmd.none
          )
          --List.take 10 (sortContrib (result model)).stats

{-
table : List Attribute -> List Html -> Html
represents a row of cells in a table:
tr : List Attribute -> List Html -> Html
represents a data cell in a table:
td : List Attribute -> List Html -> Html
-}
view : Model -> Html Msg
view model = 
    div []
    [ h2 [] [text "Summary" ]
--    , table [] [ tr [] (oneRow ["Members", "?"])
--    , tr [] (oneRow ["Countries and territories", (toString (countCountries model))])
--    , tr [] (oneRow ["Contributions", (toString (countContributions model))])
--    ]
    , table [] (matrix [ ["Members", "?"], ["Countries and territories", (toString (countCountries model))]
    ,["Contributions", (toString (countContributions model))] ])
    , h2 [] [text "By country"]
    , button [onClick Display] [text "Get from Url"]
    , bars (List.map .contributions model.stats) (List.map .country model.stats)
    , table [] (matrix (newTable (sortContrib model) ))
 -- table [] ((th [] (oneRow ["Country", "Contributions"])) :: (matrix (newTable (sortContrib model) )))
    ]

bars : List Int -> List String -> Html Msg
bars vals labels =
  pie (List.map toFloat vals) labels
    |> Chart.title "Contributions per country (in percent)"
    |> Chart.toHtml

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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    
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
    """dl {
        display: flex;
        background-color: white;
        flex-direction: column;
        width: 100%;
        max-width: 700px;
        position: relative;
        padding: 20px;
        }

        dt {
        align-self: flex-start;
        width: 100%;
        font-weight: 700;
        display: block;
        text-align: center;
        font-size: 1.2em;
        font-weight: 700;
        margin-bottom: 20px;
        margin-left: 130px;
        }

        .text {
        font-weight: 600;
        display: flex;
        align-items: center;
        height: 40px;
        width: 130px;
        background-color: white;
        position: absolute;
        left: 0;
        justify-content: flex-end;
        }

        .percentage {
        font-size: .8em;
        line-height: 1;
        text-transform: uppercase;
        width: 100%;
        height: 40px;
        margin-left: 130px;
        background: repeating-linear-gradient(
        to right,
        #ddd,
        #ddd 1px,
        #fff 1px,
        #fff 5%
        );
        
        &:after {
            content: "";
            display: block;
            background-color: #3d9970;
            width: 50px;
            margin-bottom: 10px;
            height: 90%;
            position: relative;
            top: 50%;
            transform: translateY(-50%);
            transition: background-color .3s ease;
            cursor: pointer;
        }
        &:hover,
        &:focus {
            &:after {
            background-color: #aaa; 
            }
        }
        }

        @for $i from 1 through 100 {
        .percentage-#{$i} {
            &:after {
            $value: ($i * 1%);
            width: $value;
            }
        }
        }

        html, body {
        height: 500px;
        font-family: "fira-sans-2",sans-serif;
        color: #333;
        }
    """

