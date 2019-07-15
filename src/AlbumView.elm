module AlbumView exposing
    ( view
    )

import AlbumModel exposing (Model)
import AlbumMsg exposing (Msg(..))
import AlbumCommon exposing (tagEditorUrl)

import FileList exposing (FileList)
import Urls

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Style
import Elements exposing (flatButton)


view : Model -> Html Msg
view model =
    let
        searchForm =
            Html.Styled.form [onSubmit SubmitSearch, css [Style.searchContainerStyle]]
                [ input [ placeholder "Search", onInput SearchQueryChanged ] []
                , flatButton [Style.inlineButtonStyle] [] SubmitSearch "🔍" 1.5
                ]

        networkErrorElem =
            Maybe.withDefault (div [] [])
                <| Maybe.map (\message -> p [] [ text message ]) model.networkError

        container properties =
            case model.currentList of
                Just _ ->
                    div properties
                Nothing ->
                    div
                        (properties ++ [css [Style.albumIndexContainerStyle]])

        existingListListing =
            li [] <| List.map 
                         (\(id, length, path) ->
                             ul [] [flatButton [Style.blockButtonStyle] [] (OtherFileListClicked id) path 1])
                        model.otherFileLists

        folderListing =
            li [] 
                <| List.map 
                    (\path -> 
                        ul [] [flatButton [Style.blockButtonStyle] [] (SubmitSearchFor ("/" ++ path)) path 1]
                    ) <| model.availableFolders
    in
        case model.currentList of
            Just currentList ->
                container []
                    [ networkErrorElem
                    , searchForm
                    , createThumbnailList currentList
                    ]
            Nothing ->
                container []
                    [ networkErrorElem
                    , searchForm
                    , existingListListing
                    , folderListing
                    ]


createThumbnailList : FileList -> Html Msg
createThumbnailList fileList =
    let
        amount =
            if fileList.length - 1 < 20 then
                fileList.length - 1
            else
                20

        fileIds =
            List.range 0 amount

        fileElements fileId =
            a [href <| tagEditorUrl fileList.listId fileId, css [Style.albumThumbnailStyle]]
                [ img [src <| Urls.fileListGetThumbnailUrl fileList.listId fileId] []
                ]
    in
        div [css [Style.albumThumbnailContainerStyle]]
            <| List.map fileElements fileIds


