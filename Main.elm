import Stats exposing (Model, Msg, init, update, view, subscriptions)
import Html.App as Html
import Html exposing (..)
import Html.Events exposing (onClick)

main : Program Never
main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subs
        }

subs : Model -> Sub Msg
subs model =
    Sub.none

type Msg
    = Display1
    | Display2
    | First Stats.Msg
    | FirstInit Stats.Msg

type State
    = Stat1
    | Stat2
    | Empty

type alias Model =
    { state : State 
    , stat1 : Stats.Model
    }
--INIT
initialModel : (Model, Cmd Msg)
initialModel =
    let
        (first, firstCmd) = Stats.init
    in
        ({ state = Empty
         , stat1 = first
         }
        , Cmd.map FirstInit firstCmd
        )

--UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Display1 ->
            ({ model | state = Stat1}
            , Cmd.none)
        
        Display2 ->
            ({ model | state = Stat2 }
            , Cmd.none)
        
        First msg ->
            ({ model | stat1 = fst (Stats.update msg model.stat1) }
            , Cmd.none)
        
        FirstInit firstMsg ->
            let 
                (first, cmd) = Stats.update firstMsg model.stat1
            in 
                ( { model | stat1 = first }
                , Cmd.map FirstInit cmd
                )


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