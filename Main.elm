import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput)
import Html.Attributes exposing (style, width)
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

view {editing, currentPos, root} =
  let
    center =
      cellView currentPos root
    pos =
      currentPos
  in
    div
      [ style
          [ "width" => "400px"
          ]
      ]
      [ cellView (1::pos) root, cellView (2::pos) root, cellView (3::pos) root
      , cellView (4::pos) root, cellView     pos  root, cellView (6::pos) root
      , cellView (7::pos) root, cellView (8::pos) root, cellView (9::pos) root
      , text (toString root)
      ]


cellSize : Int
cellSize = 100


(=>) = (,)


cellStyle =
  let
    cellSizePx =
      (toString cellSize) ++ "px"
  in
    style
      [ "height" => cellSizePx
      , "width" => cellSizePx
      ]


cellView pos root =
  textarea
    [ onInput (ChangeText pos), cellStyle ]
    [ text <| M.getText pos root]
