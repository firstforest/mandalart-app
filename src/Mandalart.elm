module Mandalart exposing (..)


import Html exposing (..)
import Dict
import String



-- Model
type Cell
  = Cell 
    { cellText : String
    , parent : Maybe Position
    , cells : Dict.Dict Int Cell
    }


type alias Position =
  List Int


type alias Mandalart =
  Cell


init : Mandalart
init =
  createCell Nothing


createCell : Maybe Position -> Cell
createCell parent =
  Cell 
    { cellText = ""
    , parent = parent
    , cells = Dict.empty
    }


updateText : Position -> String -> Mandalart -> Mandalart
updateText pos newText (Cell c) =
  case List.head pos of
    Nothing ->
      Cell {c | cellText = newText }

    Just i ->
      let
        defCell =
          createCell (Just pos)

        subCell =
          Maybe.withDefault defCell (Dict.get i c.cells)

        cell' =
          updateText (List.drop 1 pos) newText subCell
      in
        Cell { c | cells = (Dict.insert i cell' c.cells) }
      

getText : Position -> Mandalart -> String
getText pos (Cell {cellText, cells}) =
  case List.head pos of
    Nothing ->
      cellText

    Just i ->
      let
        defCell =
          createCell (Just pos)

        mart' =
          Maybe.withDefault defCell (Dict.get i cells)
      in
        getText (List.drop 1 pos) mart'


toStringList : Mandalart -> String
toStringList (Cell { cellText, cells }) =
  let
    cellsText =
      Dict.values cells |> List.map toStringList
        |> String.join "<br>"
  in
    cellText ++ "<br>" ++ cellsText
