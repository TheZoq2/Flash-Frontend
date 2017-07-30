module EditorMsg exposing 
    ( Msg(..)
    )

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    )


import Http
import Window
import Dom
import Navigation
import Mouse
import Scroll


type Msg
    = RequestNext
    | RequestPrev
    | RequestSave
    | NetworkError Http.Error
    | OnSaved
    | WindowResized Window.Size
    | Keypress Int
    | NewFileList Int Int Int -- selectedFile listId length
    | FileDataReceived FileData
    | UrlChanged Navigation.Location
    | ImageLoaded
    | MouseMovedOnImage Mouse.Event
    | ImageScrolled Scroll.Event
    | NoOp -- For events that are only handled because we want to prevent default
    -- Tag list specific messages
    | AddTagList
    | AddTag Int
    | StartTagAddition Int
    | ToggleTagList Int
    | RemoveTagList Int
    | ToggleTag Int Int
    | RemoveTag Int Int
    | TagTextFieldChanged String
    | CancelTagCreation
    | FocusResult (Result Dom.Error ())
