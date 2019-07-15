module EditorModel exposing
    ( Model
    , FileData
    , KeyReceiver(..)
    , FileKind(..)
    , fileKindFromExtension
    )

import Tags
import ImageViewer
import FileList exposing (fileListDecoder)
import Commands
import Browser.Navigation exposing (Key)
import Url

import Vec exposing (..)



type KeyReceiver
    = FocusNone
    | FocusTagListList
    | FocusTagList Int
    | FocusTagField Int
    | FocusTag Int Int
    | FocusCommandField Commands.CommandData




-- Data about a file that has been received from the server. The server sends more
-- data than this but we don't process that for now

type alias FileData =
    { filePath: String
    , tags: List String
    }

type FileKind
    = Image
    | Video

fileKindFromExtension : String -> FileKind
fileKindFromExtension extension =
    case extension of
        "mp4" -> Video
        "MOV" -> Video
        _ -> Image


type alias Model =
    { lastError : String
    -- The current part of the page that receives keypresses
    , keyReceiver : KeyReceiver
    -- The current size of the window
    , viewerSize: Size
    -- The currently selected tags
    , tags: Tags.TagListList
    -- The contents of the currently selected tag text input field
    , tagTextfieldContent: Maybe String
    -- The current list of files
    , fileList: Maybe FileList.FileList
    -- The id of the tag list containing the tags already on the image
    , oldTagList: Maybe Int
    -- False if the current image is not done loading
    , imageLoaded: Bool
    -- Wether or not the sidebar is shown
    , sidebarVisible: Bool
    -- The previous URL of the page
    , oldUrl: Url.Url
    -- Geometry of the current image
    , imageGeometry: ImageViewer.Geometry
    -- The current touches of the image viewer
    -- TODO: RE-enable touch support
    -- , imageTouchState: ImageViewer.TouchState
    -- The type of the currently viewed file
    , fileKind: FileKind
    -- Navigation key required to manage urls
    , navigationKey: Key
    }


