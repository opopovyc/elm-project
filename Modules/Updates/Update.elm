module Modules.Updates.Update exposing(..)

import Modules.Models.Model exposing(..)
import Modules.Updates.Msg exposing(..)
import Json.Decode as Json exposing ((:=))
import Task
import Http

--UPDATE

update : Msg a -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        FetchPass response ->
          ( { model | stats = response }
          , Cmd.none
          )

        FetchFail err ->
          ( model
          , Cmd.none
          )

        NoOp ->
          ( model
          , Cmd.none
          )
          --List.take 10 (sortContrib (result model)).stats

        BarView ->
          ( { model | state = Bar }
          , Cmd.none
          )

        PieView ->
          ( { model | state = Pie }
          , Cmd.none
          )

        StatView ->
          ( { model | state = Stat }
          , Cmd.none
          )

getUrl : String -> Json.Decoder (List a) -> Cmd (Msg a)
getUrl url decodeUrl =
    Task.perform FetchFail FetchPass (Http.get decodeUrl url)

init : Model a -> (Model a, Cmd (Msg a))
init emptyModel =
  ( emptyModel
  , getUrl emptyModel.url emptyModel.decoder
  )
