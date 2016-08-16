module Modules.Updates.Msg exposing(..)

import Http

type Msg a
    = FetchFail Http.Error
    | FetchPass (List a)
    | NoOp
    | BarView
    | PieView
    | StatView