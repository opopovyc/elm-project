module Init exposing (initialModel)

import Models.MenuState as Menu exposing (..)
import Model exposing (Model)
import Modules.Updates.Update as Stat exposing(init)
import Modules.Models.ModelDownload as Download exposing(..)
import Modules.Models.ModelContrib as Contrib exposing(..)
import Update exposing (..)

initialModel : (Model.Model, Cmd Update.Msg)
initialModel =
    let
        (first, firstCmd) = (Stat.init Contrib.emptyModel)
        (second, secondCmd) = (Stat.init Download.emptyModel)
    in
        (
            { state = Menu.Empty
            , stat1 = first
            , stat2 =  second
            }
        , Cmd.batch
            [ Cmd.map FirstInit firstCmd
            , Cmd.map SecondInit secondCmd
            ]
        )
