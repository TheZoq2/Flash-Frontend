module AlbumMsg exposing
    ( Msg (..)
    )

import Http
import FileList exposing (FileListResponse)


type Msg
    = NetworkError Http.Error
    | SearchQueryChanged String
    | SubmitSearch
    | NewFileList Int Int
    | NewFileListListing (List FileListResponse)
    | OtherFileListClicked Int
    | FileListLastSavedReceived Int Int


