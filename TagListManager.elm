module TagListManager exposing (Model, Msg, init, update, view, getTagList)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode 

--Model
type alias Model =
    {
        tags: List Tag,
        nextId: Int,

        currentTextFieldText: String
    }

type alias Tag =
    {
        id: Int,
        name: String
    }

init : Model
init = 
    Model [] 0 ""




--Returns a list of the name of all the tags in the model
getTagList : Model -> List String
getTagList model =
    List.map (\tag -> tag.name) model.tags







--Update function

type Msg
    = AddTag
    | RemoveTag Int
    | InputChanged String


update: Msg -> Model -> Model
update msg model =
    case msg of 
        AddTag ->
            {model | 
                tags = model.tags ++ [(Tag model.nextId model.currentTextFieldText)],
                nextId = model.nextId + 1,
                currentTextFieldText = ""
            }

        RemoveTag index ->
            {model | tags = List.filter (\tag -> tag.id /= index) model.tags}
        
        InputChanged text ->
            {model | currentTextFieldText=text}






--View rendering

view : Model -> Html Msg
view model =
    div []
    [
        Html.form [onSubmit AddTag 
            ]
        [
            input [placeholder "Add tag", onInput InputChanged, value model.currentTextFieldText] []
        ],
        ul [] (List.map viewTag model.tags)
    ]


--Create the HTML code for a single tag
viewTag: Tag -> Html Msg
viewTag tag =
    li []
    [
        text tag.name,

        button [onClick (RemoveTag tag.id)] [text "remove"]
    ]

main =
  Html.App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }

