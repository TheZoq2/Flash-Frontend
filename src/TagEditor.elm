module TagEditor exposing (..)

import Tags
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
import Dom


-- MODEL


type KeyReceiver
    = None
    | TagListList
    | TagList Int
    | TagField Int


type alias Model =
    { currentImage : String
    , currentImageDimensions : ( Int, Int )
    , lastError : String
    , keyReceiver : KeyReceiver
    , viewerSize: Size
    , tags: Tags.TagListList
    , tagTextfieldContent: Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" ( 0, 0 ) "" None (Size 0 0 ) Tags.emptyTagListList Nothing
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
    -- Tag list specific messages
    | AddTagList
    | AddTag Int
    | StartTagAddition Int
    | ToggleTagList Int
    | RemoveTagList Int
    | ToggleTag Int Int
    | RemoveTag Int Int
    | TagTextFieldChanged String
    | CancelTagCreation
    | FocusResult (Result Dom.Error ())

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
        --TODO: Write a general function for dealing with taglistlist messages
        AddTagList ->
            addTagList model
        AddTag id ->
            let
                newTags =
                    case model.tagTextfieldContent of
                        Just text ->
                            Tags.addTagToTagListList text id model.tags
                        Nothing ->
                            model.tags
            in
                ({model | tags = newTags} |> cancelTagCreation, Cmd.none)
        StartTagAddition id ->
            let
                newTags = Tags.startTagTextInput id model.tags
            in
                ({model | tags = newTags, keyReceiver = TagField id}, Dom.focus "tag_input_field" |> Task.attempt FocusResult)
        ToggleTagList id ->
            let
                newTags = Tags.toggleTagList id model.tags
            in
                ({model | tags = newTags}, Cmd.none)
        RemoveTagList id ->
            let
                newTags = Tags.removeTagList id model.tags
            in
                ({model | tags = newTags}, Cmd.none)
        ToggleTag listId tagId ->
            let
                newTags = Tags.toggleTagInTagListList listId tagId model.tags
            in
                ({model | tags = newTags}, Cmd.none)
        RemoveTag listId tagId ->
            let
                newTags = Tags.removeTagFromTagListList listId tagId model.tags
            in
                ({model | tags = newTags}, Cmd.none)
        CancelTagCreation ->
            let
                newModel = cancelTagCreation model
            in
                ({newModel | keyReceiver = None}, Cmd.none)
        TagTextFieldChanged text ->
            ({model | tagTextfieldContent = Just text}, Cmd.none)
        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    let
                        _ = Debug.log "Failed to find dom element when trying to focus on" id
                    in
                        (model, Cmd.none)
                Ok () ->
                    (model, Cmd.none)
        Keypress code ->
            handleKeyboardInput model code



addTagList : Model -> (Model, Cmd Msg)
addTagList model =
    let
        newTags = Tags.addTagList Tags.emptyTagList model.tags
    in
        ( {model | tags = newTags }, Cmd.none)



cancelTagCreation : Model -> Model
cancelTagCreation model =
    {model | tagTextfieldContent = Nothing, tags = Tags.cancelAddTag model.tags}




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
                case Char.fromCode code of
                    'I' ->
                        --Return to normal
                        ( {model | keyReceiver = None}, Cmd.none)
                    'A' ->
                        addTagList model
                    _ ->
                        ( model, Cmd.none )
            _ ->
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


getSelectedTags : Model -> List String
getSelectedTags model =
    Tags.selectedTags model.tags



-- VIEW

view : Model -> Html Msg
view model =
    let
        prevButton =
            flatButton [Style.BlockButton] [] RequestPrev "‹" 3

        nextButton =
            flatButton [Style.BlockButton] [] RequestNext "›" 3

        saveButton =
            flatButton [Style.BlockButton] [] RequestSave "✔" 1.5

        buttonRow =
            div [ Style.class [ Style.TagEditorButtonRow ] ] [ prevButton, nextButton, saveButton ]

        addTagList =
            flatButton [Style.WideButton, Style.BlockButton] [] AddTagList "+" 2

        additionalRightPaneClasses =
            if model.keyReceiver == TagListList then
                [ Style.TagEditorRightPaneSelected ]
            else
                []

        listMessages =
                { onAddTag = StartTagAddition
                , onRemoveList = RemoveTagList
                , onToggleList = ToggleTagList
                , onTagRemoveButton = RemoveTag
                , onTagTextClick = ToggleTag
                , onTagnameUnfocus = CancelTagCreation
                , onTagSubmit = AddTag
                , onTextChanged = TagTextFieldChanged
                }
    in
        div [ Style.class [ Style.TagEditorContainer ] ]
            <|
                [ div [ Style.class [ Style.TagEditorContentContainer], Style.styleFromSize model.viewerSize ] 
                    <| [
                        ImageViewer.imageViewerHtml
                            (model.viewerSize.width, model.viewerSize.height)
                            (0, 0)
                            1
                            model.currentImage
                        ]
                    ]
                ++ [ div [ Style.class ([ Style.TagEditorRightPane ] ++ additionalRightPaneClasses) ]
                    [ buttonRow
                    , Tags.tagListListHtml model.tags listMessages
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
