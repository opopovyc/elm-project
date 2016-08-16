module Modules.Models.ModelDownload exposing (..)

import Modules.Models.Model exposing(..)
import Json.Decode as Json exposing ((:=))

type alias Line = 
    { id : String
    , title : String
    , name : String
    , dtype : String
    , size : Int
    , downloads : Int
    }

emptyModel: Model Line
emptyModel = 
  { stats = 
      [ { id = ""
        , title = ""
        , name = ""
        , dtype = ""
        , size = 0
        , downloads = 0
        }
      ]
  , state = Empty
  , url = "https://knowledge-gateway.org/?am43v2d9"--"https://knowledge-gateway.org/?1nf8xwvd"
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
    Json.object6 Line
        ("id" := Json.string)
        ("title" := Json.string)
        ("name" := Json.string)
        ("type" := Json.string)
        ("size" := Json.int)
        ("downloads" := Json.int)
