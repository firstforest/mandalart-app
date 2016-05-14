import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onClick, onDoubleClick)
import Html.Attributes exposing (style, width, autofocus)
import Mandalart as M
import Keyboard
import Markdown


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
  , focus : Int
  , isCtrlDown : Bool
  }


init =
  (Model False [] initRoot 5 False, Cmd.none)


initRoot =
  M.init


-- Update

type Msg
  = ChangeText M.Position String
  | ChangeCenter M.Position
  | Focus Int
  | None
  | SetEditMode Bool
  | DownCtrl
  | UpCtrl


update msg model =
  case Debug.log "msg" msg of
    ChangeText pos newText ->
      ( { model | root = M.updateText pos newText model.root }
      , Cmd.none
      )

    ChangeCenter newPos ->
      ( { model
        | currentPos = newPos
        , focus = 5
        }
      , Cmd.none
      )

    Focus pos ->
      ( { model | focus = pos }
      , Cmd.none
      )

    SetEditMode b ->
      ( { model | editing = b }
      , Cmd.none
      )

    DownCtrl ->
      ( { model | isCtrlDown = True }
      , Cmd.none
      )

    UpCtrl ->
      ( { model | isCtrlDown = False }
      , Cmd.none
      )

    _ ->
      (model, Cmd.none)

-- Subscription

subscriptions model =
  if model.editing then
    let
      downs key =
        case key of
          13 -> -- enter
            if model.isCtrlDown then
              SetEditMode False
            else
              None

          17 -> -- ctrl
            DownCtrl

          _ ->
            None

      ups key =
        case key of
          17 ->
            UpCtrl

          _ ->
            None
    in
      Sub.batch [Keyboard.downs downs, Keyboard.ups ups]
  else
    let
      nextFocus d current =
        let
          t = current + d
        in
          if t < 1 then
            t + 9
          else if 9 < t then
            t - 9
          else
            t

      f key =
        case key of
          37 -> -- left
            Focus (nextFocus -1 model.focus)
          38 -> -- up
            Focus (nextFocus -3 model.focus)
          39 -> -- right
            Focus (nextFocus 1 model.focus)
          40 -> -- down
            Focus (nextFocus 3 model.focus)
          13 -> -- enter
            SetEditMode True
          32 -> -- space
            if model.focus == 5 then
              ChangeCenter (List.drop 1 model.currentPos)
            else
              ChangeCenter (model.focus :: model.currentPos)
          _ ->
            None

      ups key =
        case key of
          17 ->
            UpCtrl

          _ ->
            None
    in
      Sub.batch [Keyboard.downs f, Keyboard.ups ups]


-- View

view {editing, currentPos, root, focus} =
  let
    pos =
      currentPos

    center =
      let
        edit =
          editing && (focus == 5)
      in
        cellView pos (List.drop 1 pos) 5 edit focus root

    createView i =
      let
        edit =
            editing && focus == i
      in
        cellView (pos ++ [i]) (pos ++ [i]) i edit focus root
  in
    div []
    [
    div
      [ style
          [ "width" => (toString (cellSize * 3) ++ "px")
          ]
      ]
      [ createView 1, createView 2, createView 3
      , createView 4, center      , createView 6
      , createView 7, createView 8, createView 9
      ]
    , Markdown.toHtml [style ["float" => "left"]] (M.toStringList root)
    ]

cellSize : Int
cellSize = 200


(=>) = (,)


cellStyle =
  let
    cellSizePx =
      (toString cellSize) ++ "px"
  in
    style
      [ "height" => cellSizePx
      , "width" => cellSizePx
      , "float" => "left"
      ]

buttonStyle =
  style
    [ "width" => "auto"
    , "height" => "auto"
    ]


textareaStyle =
  style
    [ "width" => "180px"
    , "height" => "130px"
    ]


cellView pos selPos focusPos edit focus root =
  div
    [ cellStyle
    ]
    [ button
        [ onClick (ChangeCenter selPos)
        , buttonStyle
        ]
        [ text "select" ]
    , if edit then
        textarea
          [ onInput (ChangeText pos)
          , textareaStyle
          , autofocus False
          ]
          [ text <| M.getText pos root
          ]
      else
        div
          [ onClick (Focus focusPos)
          , onDoubleClick (SetEditMode True)
          , style
            [ "outline" => "solid"
            , "outline-color" => if focus == focusPos then "red" else "black"
            , "height" => ((toString (cellSize - 50)) ++ "px")
            , "width" => ((toString (cellSize - 10)) ++ "px")
            ]
          ]
          [ text <| M.getText pos root
          ]
    ]
