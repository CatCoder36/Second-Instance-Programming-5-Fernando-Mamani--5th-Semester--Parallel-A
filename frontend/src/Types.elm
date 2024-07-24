module Types exposing (..)

type alias Model =
    { selectedImage : Maybe String 
    , asciiArt : Maybe String,
    scaleFactor : Int }

type Msg
    = OpenFileDialog
    | FileSelected String
    | ReceiveFileDataUrl String
    | SetAsciiArt String
    | ValueChanged String