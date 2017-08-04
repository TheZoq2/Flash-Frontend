module EditorModel exposing
    ( Model
    , FileData
    , KeyReceiver(..)
    )

import Tags
import ImageViewer
import FileList exposing (fileListDecoder, fileListFileUrl, fileListListUrl)

import Vec exposing (..)

import WebGL.Texture



type KeyReceiver
    = None
    | TagListList
    | TagList Int
    | TagField Int
    | Tag Int Int




-- Data about a file that has been received from the server. The server sends more
-- data than this but we don't process that for now

type alias FileData =
    { filePath: String
    , tags: List String
    }


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
    , oldUrl: String
    -- Geometry of the current image
    , imageGeometry: ImageViewer.Geometry
    -- The current touches of the image viewer
    , imageTouchState: ImageViewer.TouchState
    -- The current texture used to render the image
    , imageTexture: Maybe WebGL.Texture.Texture
    }


