module Update exposing (..)

import Model exposing (Model)
import Modules.Updates.Msg as ModuleMsg exposing (..)
import Modules.Updates.Update as ModuleUpdate exposing(update)
import Modules.Models.ModelContrib as Contrib exposing(..)
import Modules.Models.ModelDownload as Download exposing(..)
import Models.MenuState exposing (..)

--type Row = Contrib.Line | Download.Line

type Msg
    = Display1
    | Display2
    | First (ModuleMsg.Msg Contrib.Line)
    | Second (ModuleMsg.Msg Download.Line)
    | FirstInit (ModuleMsg.Msg Contrib.Line)
    | SecondInit (ModuleMsg.Msg Download.Line)

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
            ({ model | stat1 = fst (ModuleUpdate.update msg model.stat1) }
            , Cmd.none)
        
        Second msg ->
            ({ model | stat2 = fst (ModuleUpdate.update msg model.stat2) }
            , Cmd.none)
        
        FirstInit firstMsg ->
            let 
                (first, cmd) = ModuleUpdate.update firstMsg model.stat1
            in 
                ( { model | stat1 = first }
                , Cmd.none --Cmd.map FirstInit cmd
                )
        
        SecondInit secondMsg ->
            let 
                (second, cmd) = ModuleUpdate.update secondMsg model.stat2
            in 
                ( { model | stat2 = second }
                , Cmd.none --Cmd.map SecondInit cmd
                )

--SUBSCRIPTIONS
subs : Model -> Sub Msg
subs model =
    Sub.none