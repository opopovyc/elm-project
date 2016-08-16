module Model exposing (Model)

import Models.MenuState as Menu exposing (..)
import Modules.Models.ModelContrib as Contrib exposing(Line)
import Modules.Models.ModelDownload as Download exposing(Line)
import Modules.Models.Model as Stat exposing(..)
--import Modules.Updates.UpdateContrib exposing(init)
--import Modules.Contributions.Stats as Stats exposing (init, Model)

type alias Model =
    { state : Menu.State 
    , stat1 : Stat.Model Contrib.Line
    , stat2 : Stat.Model Download.Line
    }