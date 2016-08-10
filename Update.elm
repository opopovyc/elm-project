module Update exposing (..)

import Model exposing (Model)
import Modules.Contributions.Stats as Stats exposing (Msg, update)
import Models.MenuState exposing (..)

type Msg
    = Display1
    | Display2
    | First Stats.Msg
    | FirstInit Stats.Msg

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


--SUBSCRIPTIONS
subs : Model -> Sub Msg
subs model =
    Sub.none