module AlbumModel exposing
    ( Model
    , init
    )

import FileList exposing (FileList, FileListSource, FileListResponse)

import AlbumMsg exposing (Msg(..))
import Requests

--Model


type alias Model =
    { searchQuery : String
    , currentList : Maybe FileList
    , networkError : Maybe String
    , otherFileLists: List (Int, Int, String)
    , availableFolders: List String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { searchQuery = ""
      , currentList = Nothing
      , networkError = Nothing
      , otherFileLists = []
      , availableFolders = []
      }
    , Cmd.batch
        [ FileList.requestFileListListing NewFileListListing NetworkError
        , Requests.requestSubdirectories
        ]
    )

