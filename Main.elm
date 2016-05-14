import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, width, autofocus)
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
  , focus : Maybe Int
  }


init =
  (Model True [] initRoot Nothing, Cmd.none)


initRoot =
  M.init


-- Update

type Msg
  = ChangeText M.Position String
  | ChangeCenter M.Position
  | Focus Int


update msg model =
  case Debug.log "msg" msg of
    ChangeText pos newText ->
      ( { model | root = M.updateText pos newText model.root }
      , Cmd.none
      )

    ChangeCenter newPos ->
      ( { model
        | currentPos = newPos
        , focus = Nothing
        }
      , Cmd.none
      )

    Focus pos ->
      ( { model | focus = Just pos }
      , Cmd.none
      )


-- Subscription

subscriptions model =
  Sub.none


-- View

view {editing, currentPos, root, focus} =
  let
    pos =
      currentPos

    center =
      let
        edit =
          case focus of
            Nothing ->
              False
            
            Just n ->
              (editing && n == 5)
      in
        cellView pos (List.drop 1 pos) 5 edit root

    createView i =
      let
        edit =
          case focus of
            Nothing ->
              False
            
            Just n ->
              (editing && n == i)
      in
        cellView (pos ++ [i]) (pos ++ [i]) i edit root
  in
    div
      [ style
          [ "width" => (toString (cellSize * 3) ++ "px")
          ]
      ]
      [ createView 1, createView 2, createView 3
      , createView 4, center      , createView 6
      , createView 7, createView 8, createView 9
      , text (toString root)
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


cellView pos selPos focusPos edit root =
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
          , style
            [ "outline" => "solid"
            , "height" => ((toString (cellSize - 50)) ++ "px")
            , "width" => ((toString (cellSize - 10)) ++ "px")
            ]
          ]
          [ text <| M.getText pos root
          ]
    ]
