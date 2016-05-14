import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput)
import Mandalart as M



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


--Model

type alias Model =
  { editing : Bool
  , currentPos : M.Position
  , root : M.Mandalart
  }


init =
  (Model False [] initRoot, Cmd.none)


initRoot =
  M.init


-- Update

type Msg
  = ChangeText M.Position String

update msg model =
  case msg of
    ChangeText pos newText ->
      ({ model | root = M.updateText pos newText model.root}
      , Cmd.none)


-- Subscription

subscriptions model =
  Sub.none


-- View

view {currentPos, root} =
  div [] <|
    [ text <| M.getText currentPos root
    , input [onInput (ChangeText currentPos)] []
    ]
