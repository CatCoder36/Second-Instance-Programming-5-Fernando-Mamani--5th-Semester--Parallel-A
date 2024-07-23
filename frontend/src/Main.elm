port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (split)
import Styles
-- Ports for reading file data
port requestFileRead : String -> Cmd msg
port receiveFileDataUrl : (String -> msg) -> Sub msg

-- Ports for send data to ImageApi
port sendFile : String -> Cmd msg
port receiveAscii : (String -> msg) -> Sub msg

type alias Model =
    { selectedImage : Maybe String 
    , asciiArt : Maybe String}

init : Model
init =
    { selectedImage = Nothing 
    , asciiArt = Nothing }

type Msg
    = OpenFileDialog
    | FileSelected String
    | ReceiveFileDataUrl String
    | SetAsciiArt String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenFileDialog ->
            (model, Cmd.none)
        FileSelected fileId ->
            let
                getUrlData = requestFileRead fileId
                sendToApi = sendFile fileId
            in
            (model, Cmd.batch [getUrlData, sendToApi])

        ReceiveFileDataUrl dataUrl ->
            ({ model | selectedImage = Just dataUrl }, Cmd.none)

        SetAsciiArt asciiArt ->
            ({ model | asciiArt = Just asciiArt }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div Styles.mainDiv
        [ input [ type_ "file", accept "image/*", id "file-input", onInput (\_ -> FileSelected "file-input"), attribute "style" "display:none;" ] []
        , label [ for "file-input", attribute "style" "cursor: pointer; background-color: #f0f0f0; padding: 10px; border-radius: 5px;" ] [ text "Selecct your image" ]
        , case model.selectedImage of
            Nothing ->
                text "No image selected"

            Just image ->
                div []
                    [ text "Selected image (view): "
                    , img [ src image, style "max-width" "600px", style "max-height" "600px" ] []
                    ]
        , case model.asciiArt of
            Nothing ->
                text "Ascii art preview" 
            Just asciiArtString ->
                asciiArtToHtml asciiArtString
        ]

asciiArtToHtml : String -> Html msg
asciiArtToHtml asciiArtString =
    let
        parts = split "<br>" asciiArtString
        textElements = List.map text parts
        withBreaks = List.intersperse (br [] []) textElements
    in
    div []
        [ text "ASCII Art: "
        , pre [ style "white-space" "pre-wrap",
                 style "font-family" "monospace",
                 style "font-size""1px" ] withBreaks
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveFileDataUrl ReceiveFileDataUrl
        , receiveAscii SetAsciiArt
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)  
        , update = update
        , view = view
        , subscriptions = subscriptions
        }