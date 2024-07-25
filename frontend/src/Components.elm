module Components exposing (..)

-- This module contains the components that are used in the view of the application.
-- The components are functions that receive the model and return the corresponding HTML.

import Html exposing (..)
import Html.Attributes exposing (..)
import Styles exposing (..)
import String exposing (split)
import Types exposing (..) 
import Html.Events exposing (onInput)

headerComponent : Model -> Html Msg
headerComponent model =
    header Styles.headerStyle 
        [ div Styles.headerContentStyle 
            [ div Styles.titleContainerStyle
                [
                    img [ src "./assets/paint.png", class "icon-style"] [],
                    h1 Styles.titleStyle [ text "Image to ASCII Art" ]
                ],
                a [ href "https://github.com/CatCoder36/Second-Instance-Programming-5-Fernando-Mamani--5th-Semester--Parallel-A.git" ] [ h3 Styles.gitHubTitleStyle [text  "GitHub" ]]
            ]]

imageSelectionComponent : Model -> Html Msg
imageSelectionComponent model =
    case model.selectedImage of
        Nothing ->
            div Styles.emptyContainerStyle
                [ h3 Styles.selectedImageTextStyle [ text "UPLOAD IMAGE" ]]
        Just image ->
            div []
                [ img [ src image, class "image-style" ] [] ]

rangeInputComponent : Model -> Html Msg
rangeInputComponent model =
   div Styles.rangeInputStyle [ input [  Html.Attributes.type_ "range", Html.Attributes.min "5", Html.Attributes.max "32", value (String.fromInt model.scaleFactor), onInput ValueChanged ] []
            , div Styles.rangeInputStyle [ text ("Diminished resolution: x " ++ String.fromInt model.scaleFactor) ]
            ]
textExplainDisminution : Html Msg
textExplainDisminution =
    div Styles.textExplainDisminutionStyle
        [ h3 Styles.titleExplainDisminutionStyle [ text "Adjust the resolution decrease" ]
        , p Styles.paragraphExplainDisminutionStyle [ text "The resolution of the image will be reduced by a factor of x times" ]
        ]

fileInputComponent : Html Msg
fileInputComponent =
    div []
        [ input [ type_ "file", accept "image/*", id "file-input", onInput (\_ -> FileSelected "file-input"), attribute "style" "display:none;" ] []
        ,  div Styles.containerButtonFileStyle
        [label [ for "file-input", attribute "style" Styles.lineLabelStyle ] [ text "Select your image" ]
        ]
        ]

asciiArtPreviewComponent : Model -> Html Msg
asciiArtPreviewComponent model =
    case model.asciiArt of
        Nothing ->
            div Styles.emptyContainerStyle
                [ h3 Styles.selectedImageTextStyle [ text "ASCII Art" ]
                ]

        Just asciiArtString ->
            asciiArtToHtml asciiArtString

asciiArtToHtml : String -> Html msg
asciiArtToHtml asciiArtString =
    let
        parts = split "<br>" asciiArtString
        textElements = List.map text parts
        withBreaks = List.intersperse (br [] []) textElements
    in
    div []
        [ pre Styles.asciiArtStyle withBreaks
        ]

principalTitleComponent : Html msg
principalTitleComponent =
    div Styles.principalTitleContainerStyle
        [ h1 Styles.titleStyle [ text "ASCII-Generator" ]
        , h2 [] [text "Upload an image for conversion to ascii artwork"]
        ]