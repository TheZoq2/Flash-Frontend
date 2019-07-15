module EditorMsg exposing 
    ( Msg(..)
    )

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    )

import Http
import Browser
import Browser.Dom
import Scroll
import Url
import Mouse
import Json.Decode


type Msg
    = RequestNext
    | RequestPrev
    | RequestSave
    | RequestId Int
    | NetworkError Http.Error
    | OnSaved
    | WindowResized Int Int
    | Keypress String
    | NewFileList Int Int Int -- selectedFile listId length
    | FileDataReceived FileData
    | UrlChanged Url.Url
    | UrlRequest Browser.UrlRequest
    | ImageLoaded
    | CommandCanceled
    | CommandInput String
    | ToggleSidebar
    -- Image view specific messages
    | MouseMovedOnImage Mouse.Event
    | ImageScrolled Scroll.Event
    -- TODO: Re-add touch support
    -- | ImageTouchStart Touch.Event
    -- | ImageTouchMove Touch.Event
    -- | ImageTouchEnd Touch.Event
    | NoOp -- For events that are only handled because we want to prevent default
    -- Tag list specific messages
    | AddTagList
    | FinnishAddTag Int
    | AddTag Int String
    | StartTagAddition Int
    | ToggleTagList Int
    | RemoveTagList Int
    | RemoveTagByName String
    | ToggleTagByName String
    | ToggleTag Int Int
    | RemoveTag Int Int
    | TagTextFieldChanged String
    | CancelTagCreation
    | FocusResult (Result Browser.Dom.Error ())




