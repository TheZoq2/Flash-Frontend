module Tags exposing 
    (Tag
    , TagList
    , emptyTagList
    , addTagToList
    , removeTag
    , tagListSelectedTags
    , selectedTags
    , addTagList
    , removeTagList
    , emptyTagListList
    , removeTagFromTagListList
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

tagListSelectedTags : TagList -> List String
tagListSelectedTags list =
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





-- TagListLists
type alias TagListList =
    { nextId: Int
    , tagLists: Dict Int TagList
    }

emptyTagListList =
    TagListList 0 Dict.empty




--Adds a tag list to a tag list list

addTagList : TagList -> TagListList -> TagListList
addTagList addedList targetList =
    { targetList 
        | tagLists = Dict.insert targetList.nextId addedList targetList.tagLists
        , nextId = targetList.nextId + 1
        }

removeTagList : Int -> TagListList -> TagListList
removeTagList listId tagListList =
    let
        filterFunction key value =
            key /= listId
    in
        { tagListList | tagLists = Dict.filter filterFunction tagListList.tagLists }




-- Runs a function on a specified tag list

runOnTagList : (TagList -> TagList) -> Int -> TagListList -> TagListList
runOnTagList fn targetId tagListList =
    let
        mapFunction key value = 
            if key == targetId then
                fn value
            else
                value
    in
        { tagListList | tagLists = Dict.map mapFunction tagListList.tagLists }



-- Adds a tag to a tag list inside a tag list list

addTagToTagListList : String -> Int -> TagListList -> TagListList
addTagToTagListList tagText listId listList =
    runOnTagList (addTagToList tagText) listId listList




-- Removes the tag with tagId from the list with listId

removeTagFromTagListList : Int -> Int -> TagListList -> TagListList
removeTagFromTagListList listId tagId tagListList = 
    runOnTagList (removeTag tagId) listId tagListList




selectedTags : TagListList -> List String
selectedTags list =
    Dict.foldl (\_ value acc -> acc ++ tagListSelectedTags value) [] list.tagLists



tagListListHtml :
    TagListList 
   -> (Int -> msg) 
   -> (Int -> msg) 
   -> (Int -> Int -> msg) 
   -> (Int -> Int -> msg) 
   -> Html msg
tagListListHtml 
        tagListList
        onRemoveButton
        onDisableButton
        onTagRemoveButton
        onTagTextClick
    =
    let
        foldFunction =
            (\id value acc -> 
                acc ++ [(value, (onTagTextClick id), (onTagRemoveButton id))])

        tagLists =
            Dict.foldl foldFunction [] tagListList.tagLists
    in
        ul []
            <| List.map (\tag onText onRemove -> tagListHtml tag onText onRemove) tagLists
