import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Http


main = Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }

type alias Line = 
    { country : String
    , contributions : Int
    }

type alias Model = 
    { stats : List Line
    }
initialModel : Model
initialModel = 
    { stats = 
        [ { country = ""
          , contributions = 0
          }
        ]
    }

{-
decodeModel : Json.Decode.Value -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeValue modelDecoder modelJson
-}

lineDecoder : Json.Decode.Decoder Line
lineDecoder = 
    Json.Decode.Pipeline.decode Line
        |> Json.Decode.Pipeline.required "country" Json.Decode.string
        |> Json.Decode.Pipeline.required "contributions" Json.Decode.int

modelDecoder : Json.Decode.Decoder Model
modelDecoder = 
    Json.Decode.Pipeline.decode Model
        |> Json.Decode.Pipeline.required "stats" (Json.Decode.list lineDecoder)

{- String for now, normally some other type of Json values 
Json.Decode.decodeString modelDecoder-}
decodeModel : String -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeString modelDecoder modelJson

{-can't count members from this input?-}
countCountries : Model -> Int
countCountries model =
    List.length model.stats

countContributions : Model -> Int
countContributions model = 
    List.map .contributions model.stats |> List.sum

type Msg
    = Display
    | Restart
    | Expand

result : Model -> Model
result model = 
    Result.withDefault model (decodeModel newString)

update : Msg -> Model -> Model
update msg model =
    case msg of
        Display ->
            --here we change the model and we don't want that
            { model | stats = List.take 10 (sortContrib (result model)).stats }
        Restart ->
            initialModel
        Expand ->
            result model

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
    [ button [onClick Display] [text "display statistics"]
    , h2 [] [text "Summary" ]
--    , table [] [ tr [] (oneRow ["Members", "?"])
--    , tr [] (oneRow ["Countries and territories", (toString (countCountries model))])
--    , tr [] (oneRow ["Contributions", (toString (countContributions model))])
--    ]
    , table [] (matrix [ ["Members", "?"], ["Countries and territories", (toString (countCountries model))]
    ,["Contributions", (toString (countContributions model))] ])
    , h2 [] [text "By country"]
    , button [onClick Expand] [ text "Show all" ]
    , button [onClick Display] [ text "Show top 10" ]
    , div []
        [ node "style" [ type' "text/css" ] [text styles]
        , barWithTable
        ]
    , table [] (matrix (newTable (sortContrib model) ))
 -- table [] ((th [] (oneRow ["Country", "Contributions"])) :: (matrix (newTable (sortContrib model) )))
    ]
barWithTable : Html Msg
barWithTable = 
    dl [] 
        [ dt [] [ text "Contributions per country (in percent)"]
        , dd [class "percentage percentage-11"] 
            [ span [class "text"] [text "IE 11: 11.33%"]]
        , dd [class "percentage percentage-49"] 
            [ span [class "text"] [text "Chrome: 49.77%"]]
        , dd [class ["percentage" => "percentage-16"] ] 
            [ span [class "text"] [text "Firefox: 16.09%"]]
        , dd [class "percentage percentage-5"] 
            [ span [class "text"] [text "Safari: 5.41%"]]
        , dd [class "percentage percentage-2"] 
            [ span [class "text"] [text "Opera: 1.62%"]]
        , dd [class "percentage percentage-2"] 
            [ span [class "text"] [text "Android 4.4: 2%"]]
        ]
        
{-
HTML for bars:
<dl>
  <dt>
    Contributions per country (in percent)
  </dt>
  <dd class="percentage percentage-11"><span class="text">IE 11: 11.33%</span></dd>
  <dd class="percentage percentage-49"><span class="text">Chrome: 49.77%</span></dd>
  <dd class="percentage percentage-16"><span class="text">Firefox: 16.09%</span></dd>
  <dd class="percentage percentage-5"><span class="text">Safari: 5.41%</span></dd>
  <dd class="percentage percentage-2"><span class="text">Opera: 1.62%</span></dd>
  <dd class="percentage percentage-2"><span class="text">Android 4.4: 2%</span></dd>
</dl>

dl [] 
            [ dt [] [ text "Contributions per country (in percent)"]
            , dd [class "percentage percentage-11"] 
                [ span [class "text"] [text "IE 11: 11.33%"]]
            , dd [class "percentage percentage-49"] 
                [ span [class "text"] [text "Chrome: 49.77%"]]
            , dd [class "percentage percentage-16"] 
                [ span [class "text"] [text "Firefox: 16.09%"]]
            , dd [class "percentage percentage-5"] 
                [ span [class "text"] [text "Safari: 5.41%"]]
            , dd [class "percentage percentage-2"] 
                [ span [class "text"] [text "Opera: 1.62%"]]
            , dd [class "percentage percentage-2"] 
                [ span [class "text"] [text "Android 4.4: 2%"]]
            ]
-}

--barView : 

{-sortContrib : Model -> Model
sortContrib model =
    let
        flippedComparison a b =
            case compare a b of
                LT -> GT
                EQ -> EQ
                GT -> LT
    in
        List.sortWith flippedComparison (List.map .contributions model.stats) 
-}

--sort model by contributions (potentially bz a given attributed)
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

