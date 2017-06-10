module Main exposing (..)

import Tags
import Style
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import FileList exposing (FileList, fileListUrl)
import Elements exposing (flatButton)

-- Constants
filesPerPage = 20

--Model


type alias Model =
    { searchQuery : String
    , currentList : Maybe FileList
    , page: Maybe Int
    , networkError : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { searchQuery = ""
      , currentList = Nothing
      , page = Nothing
      , networkError = Nothing
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
            (Http.get url FileList.decodeNewFileList)



--View


view : Model -> Html Msg
view model =
    let
        searchForm =
            Html.form [onSubmit SubmitSearch]
                [ input [ placeholder "Search", onInput SearchQueryChanged ] []
                , flatButton [] [] SubmitSearch "Search" 1
                ]

        networkErrorElem =
            Maybe.withDefault (div [] []) 
                <| Maybe.map (\message -> p [] [ text message ]) model.networkError 
    in
        div []
            [ searchForm
            , createThumbnailList model
            , networkErrorElem
            ]


createThumbnailList : Model -> Html Msg
createThumbnailList model =
    case model.currentList of
        Just fileList ->
            let
                fileIds =
                    List.range 0 fileList.length

                fileUrls = List.map (fileListUrl [] "get_thumbnail" fileList.listId) fileIds

            in
                div []
                    <| List.map (\url -> img [src url] []) fileUrls
        Nothing ->
            p [] [text "No results"]





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
