module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

mainDiv : List (Attribute msg)
mainDiv =
    [ style "width" "100%"
    , style "min-height" "100vh"
    , style "padding" "0px"
    , style "margin" "0px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]