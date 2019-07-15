module Main exposing (..)

import AlbumModel exposing (Model, init)
import AlbumMsg exposing (Msg(..))
import AlbumView exposing (view)
import AlbumCommon exposing (tagEditorUrl)
import Requests exposing (checkHttpAttempt)

import Html.Styled exposing (..)
import Html.Styled
import Http
import FileList exposing (FileList, FileListSource, FileListResponse)
import Json.Decode
import Browser
import Browser.Navigation as Navigation
import Urls





--Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchQueryChanged query ->
            ({model | searchQuery = query}, Cmd.none)
        SubmitSearch ->
            (model, submitSearch model.searchQuery)
        SubmitSearchFor query ->
            (model, submitSearch query)
        NetworkError err ->
            ( { model | networkError = Just <| Debug.toString err }, Cmd.none )
        NewFileList id length ->
            ( { model | currentList = Just <| FileList.new id length }, Cmd.none )
        NewFileListListing fileLists ->
            updateFileListListings model fileLists
        OtherFileListClicked id ->
            onOtherFileListClicked model id
        FileListLastSavedReceived listId index ->
            (model, Navigation.load <| tagEditorUrl listId index)
        AvailableFoldersReceived folders ->
            ({model | availableFolders = folders}, Cmd.none)


updateFileListListings : Model -> List FileListResponse -> (Model, Cmd Msg)
updateFileListListings model fileLists =
    let
        otherFileLists = 
            List.filterMap (\{id, length, source} ->
                case source of
                    FileList.Folder path ->
                        Just (id, length, path)
                    FileList.Search ->
                        Nothing
                ) fileLists
    in
        ({ model | otherFileLists = otherFileLists}, Cmd.none)


onOtherFileListClicked : Model -> Int -> (Model, Cmd Msg)
onOtherFileListClicked model id =
    ( model
    , Http.send
        (checkHttpAttempt <| FileListLastSavedReceived id)
        (Http.get (Urls.fileListLastSavedIndexUrl id) Json.Decode.int)
    )


submitSearch : String -> Cmd Msg
submitSearch text =
    Http.send
        (checkHttpAttempt (\val -> NewFileList val.id val.length))
        (Http.get (Urls.searchUrl text) FileList.fileListDecoder)






--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = (\model -> Browser.Document "Flash album" [toUnstyled <| view model])
        , subscriptions = subscriptions
        }



