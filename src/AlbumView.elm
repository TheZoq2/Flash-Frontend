module AlbumView exposing
    ( view
    )

import AlbumModel exposing (Model)
import AlbumMsg exposing (Msg(..))
import AlbumCommon exposing (tagEditorUrl)

import FileList exposing (FileList)
import Urls

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style
import Elements exposing (flatButton)


view : Model -> Html Msg
view model =
    let
        searchForm =
            Html.form [onSubmit SubmitSearch, Style.class [Style.SearchContainer]]
                [ input [ placeholder "Search", onInput SearchQueryChanged ] []
                , flatButton [Style.InlineButton] [] SubmitSearch "🔍" 1.5
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
                        (properties ++ [Style.class [Style.AlbumIndexContainer]])

        existingListListing =
            li [] <| List.map 
                         (\(id, length, path) ->
                             ul [] [flatButton [Style.BlockButton] [] (OtherFileListClicked id) path 1])
                        model.otherFileLists
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
            a [href <| tagEditorUrl fileList.listId fileId, Style.class [Style.Thumbnail]]
                [ img [src <| Urls.fileListGetThumbnailUrl fileList.listId fileId] []
                ]
    in
        div [Style.class [Style.AlbumThumbnailContainer]]
            <| List.map fileElements fileIds


