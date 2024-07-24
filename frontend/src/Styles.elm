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
    , style "justify-content" "flex-start"
    , style "align-items" "center"
    , style "gap" "10px"
    ]

headerStyle : List (Attribute msg)
headerStyle =
    [  style "border-radius" "5px"
    , style "margin-bottom" "10px"
    , style "width" "100%"
    , style "display" "flex"
    , style "justify-content" "space-between"
    ]

headerContentStyle : List (Attribute msg)
headerContentStyle =
    [ style "display" "flex"
    , style "justify-content" "space-between"
    , style "align-items" "center"
    , style "width" "100%"
    , style "padding-left" "100px"
    , style "padding-right" "100px"
    ]


titleStyle : List (Attribute msg)
titleStyle =
    [ style "font-family" "'Poppins', sans-serif"]

containerImageStyle : List (Attribute msg)
containerImageStyle =
    [ style "border-radius" "10px"
    , style "border" "1px solid #8e00ec"
    , style "max-width" "600px"
    , style "max-height" "600px"
    ]

selectedImageTextStyle : List (Attribute msg)
selectedImageTextStyle =
    [style "padding-right" "10px"
    , style "padding-left" "10px"
    , style "font-family" "'Poppins', sans-serif"
    ]

rangeInputStyle : List (Attribute msg)
rangeInputStyle =
    [ style "margin-top" "10px"
    , style "margin-bottom" "10px"
    , style "font-family" "'Poppins', sans-serif"
    , style "text-align" "center" 
    ]

containerButtonFileStyle : List (Attribute msg)
containerButtonFileStyle =
    [ style "margin-top" "10px"
    , style "margin-bottom" "10px"]

asciiArtStyle : List (Attribute msg)
asciiArtStyle =
    [ style "white-space" "pre-wrap"
    , style "font-family" "monospace"
    , style "font-size" "10px"
    ]

iconHeaderStyle : List (Attribute msg)
iconHeaderStyle =
    [ style "width" "35px"
    , style "height" "35px"
    ]

titleContainerStyle : List (Attribute msg)
titleContainerStyle =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "gap" "10px"
    ]

lineLabelStyle : String  
lineLabelStyle = 
    "cursor: pointer; background-color: #8e00ec; border-radius: 5px; font-family: 'Poppins', sans-serif; padding: 10px; box-shadow: 0px 0px 5px 0px #8e00ec; color: white;"

