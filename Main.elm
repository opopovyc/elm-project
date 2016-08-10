import Html.App as Html
import Init exposing(initialModel)
import Update exposing(update, subs)
import View exposing(view)

main : Program Never
main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subs
        }
