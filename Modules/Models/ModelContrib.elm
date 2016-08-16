module Modules.Models.ModelContrib exposing (..)

import Modules.Models.Model exposing(..)
import Json.Decode as Json exposing ((:=))

type alias Line = 
    { country : String
    , contributions : Int
    }

emptyModel: Model Line
emptyModel = 
  { stats = 
      [ { country = ""
        , contributions = 0
        }
      ]
  , state = Empty
  , url = "https://knowledge-gateway.org/file2.axd/e1894e44-7d5c-4035-899a-a32c23f119c6/sample_contribution_stats.json"
  , decoder = decodeUrl
  }

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