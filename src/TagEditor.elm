module TagEditor exposing (..)

import TagListManager
import Style
import ImageViewer
import Vec exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char
import Css
import Elements exposing (flatButton)


-- MODEL


type KeyReceiver
    = None
    | TagListList


type alias Model =
    { currentImage : String
    , currentImageDimensions : ( Int, Int )
    , lastError : String
    , keyReceiver : KeyReceiver
    , viewerSize: Size
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" ( 0, 0 ) "" None (Size 0 0 )
    , Cmd.batch
        [ requestNewImage Current
        , Task.perform WindowResized Window.size
        ]
    )



-- UPDATE


type Msg
    = RequestNext
    | RequestPrev
    | RequestCurrent
    | RequestSave
    | NetworkError Http.Error
    | NewImageReceived ImageResponse
    | OnSaved String
    | WindowResized Window.Size
    | Keypress Int
    | AddTagList
    --| ToggleTag Int
    --| RemoveTag Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestNext ->
            ( model, requestNewImage Next )

        RequestPrev ->
            ( model, requestNewImage Prev )

        RequestCurrent ->
            ( model, requestNewImage Current )

        RequestSave ->
            ( model, requestSaveImage (getSelectedTags model) )

        OnSaved _ ->
            ( model, requestNewImage Next )

        NetworkError e ->
            let
                _ =
                    Debug.log "Network error" e
            in
                ( model, Cmd.none )

        NewImageReceived response ->
            let
                currentImage =
                    "" ++ response.filePath

            in
                ({ model
                        | currentImage = currentImage
                    }
                , Cmd.none)

        WindowResized size ->
            let
                viewerSize =
                    Size ((toFloat size.width) - Style.totalSidebarSize) (toFloat size.height)

            in
                ( { model | viewerSize = viewerSize }, Cmd.none )
            --(model, Cmd.none)
        AddTagList ->
            ( model, Cmd.none)

        Keypress code ->
            handleKeyboardInput model code


handleKeyboardInput : Model -> Int -> ( Model, Cmd Msg )
handleKeyboardInput model code =
    let
        _ =
            Debug.log "" (Char.fromCode code)
    in
        case model.keyReceiver of
            None ->
                case Char.fromCode code of
                    'L' ->
                        --Next
                        ( model, requestNewImage Next )

                    'H' ->
                        --Previous
                        ( model, requestNewImage Prev )

                    'S' ->
                        --Save
                        ( model, requestSaveImage (getSelectedTags model) )

                    'T' ->
                        --Modify tags
                        ( { model | keyReceiver = TagListList }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            TagListList ->
                --TODO Handle input
                (model, Cmd.none)


subComponentOwnsKeyboard : Model -> Bool
subComponentOwnsKeyboard _ =
    False


type ImageDirection
    = Next
    | Prev
    | Current


checkHttpAttempt : Result Http.Error ImageResponse -> Msg
checkHttpAttempt res =
    case res of
        Ok val ->
            NewImageReceived val
        Err e ->
            NetworkError e


requestNewImage : ImageDirection -> Cmd Msg
requestNewImage direction =
    let
        action =
            case direction of
                Next ->
                    "next"

                Prev ->
                    "prev"

                Current ->
                    "current"

        url =
            "http://localhost:3000/list?action=" ++ action
    in
        Http.send checkHttpAttempt (Http.get url decodeNewImage)


requestSaveImage : List String -> Cmd Msg
requestSaveImage tags =
    let
        --Encode the tag list
        tagsJson =
            List.map Json.Encode.string tags

        --TODO: Make surre X-Origin requests are allowed
        url =
            "http://localhost:3000/list?action=save&tags=" ++ toString tagsJson
    in
        Http.send checkHttpAttempt (Http.get url decodeNewImage)


type alias ImageResponse =
    { filePath : String
    , tags : List String
    }


decodeNewImage : Json.Decode.Decoder ImageResponse
decodeNewImage =
    let
        decodeDimensions =
            Json.Decode.map2 (,) (index 0 Json.Decode.int) (index 1 Json.Decode.int)

        decodeMsg =
            Json.Decode.map2 ImageResponse
                (field "file_path" Json.Decode.string)
                (field "tags" (Json.Decode.list Json.Decode.string))
    in
        decodeMsg


--TODO: Implement
getSelectedTags : Model -> List String
getSelectedTags model =
    --TagListList.getSelectedTags model.tagListList
    []



-- VIEW

view : Model -> Html Msg
view model =
    let
        prevButton =
            flatButton [] [] RequestNext "‹" 3

        nextButton =
            flatButton [] [] RequestPrev "›" 3

        saveButton =
            flatButton [] [] RequestSave "✔" 1.5

        buttonRow =
            div [ Style.class [ Style.TagEditorButtonRow ] ] [ prevButton, nextButton, saveButton ]

        addTagList =
            flatButton [Style.WideButton] [] RequestSave "+" 2

        additionalRightPaneClasses =
            if model.keyReceiver == TagListList then
                [ Style.TagEditorRightPaneSelected ]
            else
                []
    in
        div [ Style.class [ Style.TagEditorContainer ] ]
            [ div [ Style.class [ Style.TagEditorContentContainer], Style.styleFromSize model.viewerSize ] 
                [
                    ImageViewer.imageViewerHtml
                        (model.viewerSize.width, model.viewerSize.height)
                        (0, 0)
                        1
                        model.currentImage
                    ]
            , div [ Style.class ([ Style.TagEditorRightPane ] ++ additionalRightPaneClasses) ]
                [ buttonRow
                , addTagList
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResized
        , Keyboard.downs Keypress
        ]



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
