module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Html exposing (s)

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
    , style "background-color" "#db4d5c"
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
    [ style "font-family" "Pacifico, 'Ubuntu Medium', 'Ubuntu', sans-serif, 'Times New Roman'"
    , style "color" "white"
    ]

containerImageStyle : List (Attribute msg)
containerImageStyle =
    [ style "border-radius" "10px"
    , style "border" "1px solid white"
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
    , style "color" "white"
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
    , style "max-width" "600px"
    , style "max-height" "600px"
    , style "overflow" "auto"
    , style "border-radius" "5px"
    , style "color" "black"
    , style "background-color" "white"
    , style "border" "1px solid white"
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
    , style "border-radius" "5px"

    ]

containerImageAndAsciiStyle : List (Attribute msg)
containerImageAndAsciiStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "space-between"
    , style "align-items" "center"
    , style "gap" "50px"
    ]
lineLabelStyle : String  
lineLabelStyle = 
    "cursor: pointer; background-color: white; border-radius: 5px; font-family: 'Poppins', sans-serif; padding: 10px; box-shadow: 0px 0px 5px 0px white; color: black;"

emptyContainerStyle : List (Attribute msg)
emptyContainerStyle =
    [ style "width" "348px"
    , style "height" "240px"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "border-radius" "10px"
    , style "border" "1px solid white"
    , style "background-color" "white"
    ]

principalTitleContainerStyle : List (Attribute msg)
principalTitleContainerStyle =
    [ style "font-family" "Pacifico, 'Ubuntu Medium', 'Ubuntu', sans-serif, 'Times New Roman'"
    , style "color" "white"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "flex-direction" "column"
    ]

gitHubTitleStyle : List (Attribute msg)
gitHubTitleStyle =
    [ style "font-family" "'Poppins', sans-serif"
    , style "color" "white"
    , style "width" "auto"
    ]

textExplainDisminutionStyle : List (Attribute msg)
textExplainDisminutionStyle =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]

titleExplainDisminutionStyle : List (Attribute msg)
titleExplainDisminutionStyle =
    [ style "font-family" "'Poppins', sans-serif"
    , style "color" "white"
    , style "width" "auto"
    , style "margin" "0px"
    ]

paragraphExplainDisminutionStyle : List (Attribute msg)
paragraphExplainDisminutionStyle =
    [ style "font-family" "'Poppins', sans-serif"
    , style "color" "white"
    , style "width" "auto"
    , style "margin" "0px"
    ]