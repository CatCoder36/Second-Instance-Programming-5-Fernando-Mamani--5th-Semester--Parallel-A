module Components exposing (..)

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
                h3 [] [text "Fernando Mauricio Mamani Navarro"]
            ]]

imageSelectionComponent : Model -> Html Msg
imageSelectionComponent model =
    case model.selectedImage of
        Nothing ->
            div containerImageStyle
                [ h3 Styles.selectedImageTextStyle [ text "Selected image" ]]
        Just image ->
            div []
                [ img [ src image, class "image-style" ] [] ]

rangeInputComponent : Model -> Html Msg
rangeInputComponent model =
   div Styles.rangeInputStyle [ input [  Html.Attributes.type_ "range", Html.Attributes.min "5", Html.Attributes.max "32", value (String.fromInt model.scaleFactor), onInput ValueChanged ] []
            , div Styles.rangeInputStyle [ text ("Scale: " ++ String.fromInt model.scaleFactor) ]
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
            text "" 
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