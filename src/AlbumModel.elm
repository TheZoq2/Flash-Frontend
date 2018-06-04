module AlbumModel exposing
    ( Model
    , init
    )

import FileList exposing (FileList, FileListSource, FileListResponse)

import AlbumMsg exposing (Msg(..))

--Model


type alias Model =
    { searchQuery : String
    , currentList : Maybe FileList
    , networkError : Maybe String
    , otherFileLists: List (Int, Int, String)
    }


init : ( Model, Cmd Msg )
init =
    ( { searchQuery = ""
      , currentList = Nothing
      , networkError = Nothing
      , otherFileLists = []
      }
    , FileList.requestFileListListing NewFileListListing NetworkError
    )

