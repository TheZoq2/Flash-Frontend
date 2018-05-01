module Main exposing (..)

import AlbumModel exposing (Model, init)
import AlbumMsg exposing (Msg(..))
import AlbumView exposing (view)
import AlbumCommon exposing (tagEditorUrl)
import Requests exposing (checkHttpAttempt)

import Html exposing (..)
import Html
import Http
import FileList exposing (FileList, FileListSource, FileListResponse, fileListFileUrl)
import Json.Decode
import Navigation




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
            ( { model | networkError = Just <| toString err }, Cmd.none )
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
        (Http.get (FileList.fileListListUrl [] "list_last_saved_index" id) Json.Decode.int)
    )


submitSearch : String -> Cmd Msg
submitSearch text =
    let
        url =
            "search?query=" ++ text
    in
        Http.send
            (checkHttpAttempt (\val -> NewFileList val.id val.length))
            (Http.get url FileList.fileListDecoder)






--Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



