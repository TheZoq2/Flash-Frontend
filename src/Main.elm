module Main exposing (..)

import Style
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import FileList exposing (FileList, FileListSource, fileListUrl)
import Elements exposing (flatButton)


--Model


type alias Model =
    { searchQuery : String
    , currentList : Maybe FileList
    , networkError : Maybe String
    , otherFileLists: List (Int, FileListSource)
    }


init : ( Model, Cmd Msg )
init =
    ( { searchQuery = ""
      , currentList = Nothing
      , networkError = Nothing
      , otherFileLists = []
      }
    , Cmd.none
    )



--Update


type Msg
    = NetworkError Http.Error
    | SearchQueryChanged String
    | SubmitSearch
    | NewFileList Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchQueryChanged query ->
            ({model | searchQuery = query}, Cmd.none)
        SubmitSearch ->
            (model, submitSearch model.searchQuery)
        NetworkError err ->
            ( { model | networkError = Just <| toString err }, Cmd.none )
        NewFileList id length ->
            ( { model | currentList = Just <| FileList.new id length }, Cmd.none )


checkHttpAttempt : (a -> Msg) -> Result Http.Error a -> Msg
checkHttpAttempt func res=
    case res of
        Ok val ->
            func val
        Err e ->
            NetworkError e


submitSearch : String -> Cmd Msg
submitSearch text =
    let
        url =
            "search?query=" ++ text
    in
        Http.send
            (checkHttpAttempt (\val -> NewFileList val.id val.length))
            (Http.get url FileList.fileListDecoder)



--View


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
    in
        container []
            [ networkErrorElem
            , searchForm
            , createThumbnailList model.currentList
            ]


createThumbnailList : Maybe FileList -> Html Msg
createThumbnailList fileList =
    let
        noList = 
            p [] [text "No results"]
    in
    case fileList of
        Nothing ->
            noList
        --TODO Match length=0
        Just fileList ->
            let
                amount =
                    if fileList.length - 1 < 20 then
                        fileList.length - 1
                    else
                        20

                fileIds =
                    List.range 0 amount

                fileElements fileId =
                    let
                        editorUrl = "tag_editor.html#list/"
                                  ++ (toString fileList.listId)
                                  ++ "/file/"
                                  ++ (toString fileId)
                    in
                        a [href editorUrl, Style.class [Style.Thumbnail]]
                            [ img [src <| fileListUrl [] "get_thumbnail" fileList.listId fileId] []
                            ]
            in
                div [Style.class [Style.ThumbnailContainer]]
                    <| List.map fileElements fileIds





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



