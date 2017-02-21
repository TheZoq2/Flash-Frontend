module TagListManager exposing 
    (Tag
    , TagList
    , emptyTagList
    , addTagToList
    , removeTag
    , selectedTags
    )

import Html exposing (..)
import Html.Events exposing (..)

import Elements exposing (flatButton)
import Style
import Dict exposing (Dict)

type alias Tag =
    { text: String
    , selected: Bool
    }

newTag : String -> Tag
newTag text =
    Tag text True


--Generates the html to display a single tag
htmlFromTag : Tag -> msg -> msg -> Html msg
htmlFromTag tag onTextClick onRemoveButton =
    span []
        [ p [onClick (onTextClick)] [text tag.text]
        , flatButton [Style.InlineButton] [] (onRemoveButton) "x" 1
        ]



htmlFromListOfTags : List (Tag, msg, msg) -> Html msg
htmlFromListOfTags list =
    ul []
        <| List.map 
            (\(tag, onTextClick, onRemoveButton) -> li [] [htmlFromTag tag onTextClick onRemoveButton]) 
            list





-- TagLists

type alias TagList =
    { nextId: Int
    , tags: Dict Int Tag
    , enabled: Bool
    }


emptyTagList : TagList
emptyTagList =
    TagList 0 Dict.empty True



-- Add a tag to a tag list

addTagToList : String -> TagList -> TagList
addTagToList tagText list=
    {list | tags = Dict.insert list.nextId (newTag tagText) list.tags, nextId = list.nextId + 1}


removeTag : Int -> TagList -> TagList
removeTag id list =
    {list | tags = Dict.remove id list.tags}




-- Returns a list of the text of all enabled tags in a tag list

selectedTags : TagList -> List String
selectedTags list =
    let
        tagList = Dict.foldl (\_ tag list -> list ++ [tag]) [] list.tags
    in
        List.filter (\tag -> tag.selected) tagList
        |> List.map (\tag -> tag.text)



-- Returns the html for a given tag

tagListHtml : TagList -> (Int -> msg) -> (Int -> msg) -> Html msg
tagListHtml list onTextClick onRemoveButton =
    Dict.foldl 
        (\key value acc -> acc ++ [(value, (onTextClick key), (onRemoveButton key))])
        []
        list.tags
    |> htmlFromListOfTags





