module Tags exposing 
    (Tag
    , TagList
    , TagListList
    , TagListMessages
    , SelectedTag(..)
    , emptyTagList
    , addTagToList
    , addTagsToList
    , removeTag
    , tagListSelectedTags
    , selectedTags
    , addTagList
    , removeTagList
    , emptyTagListList
    , removeTagFromTagListList
    , addTagToTagListList
    , toggleTagList
    , toggleTagInTagListList
    , startTagTextInput
    , cancelAddTag
    , getNthTagListId
    , getNthTag
    , indicesOfTag
    , allTags
    )


import Elements exposing (flatButton)
import Style
import Dict exposing (Dict)

import List.Extra

type alias Tag =
    { text: String
    , enabled: Bool
    }

newTag : String -> Tag
newTag text =
    Tag text True


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

addTagsToList : List String -> TagList -> TagList
addTagsToList tags oldList =
    List.foldl addTagToList oldList tags


removeTag : Int -> TagList -> TagList
removeTag id list =
    {list | tags = Dict.remove id list.tags}



runOnTag : (Tag -> Tag) -> Int -> TagList -> TagList
runOnTag function targetId list=
    let
        mapFunction id tag =
            if targetId == id then
                function tag
            else
                tag
    in
        { list | tags = Dict.map mapFunction list.tags }


toggleTag : Int -> TagList -> TagList
toggleTag id list =
    runOnTag (\tag -> { tag | enabled = not tag.enabled }) id list



-- Returns a list of the text of all enabled tags in a tag list

tagListSelectedTags : TagList -> List String
tagListSelectedTags list =
    tagListFilterTags (\tag -> tag.enabled) list


-- Returns all tags in the specified tag list

tagListAllTags : TagList -> List String
tagListAllTags list =
    tagListFilterTags (\_ -> True) list



-- Returns all the tag names where the tag matches the filter

tagListFilterTags : (Tag -> Bool) -> TagList -> List String
tagListFilterTags filter list =
    List.map (\tag -> tag.text)
        <| List.filter filter
        <| Dict.foldl (\_ tag list -> list ++ [tag]) [] list.tags





-- TagListLists
type alias TagListList =
    { nextId: Int
    , tagLists: Dict Int TagList
    , textFieldTargetId: Maybe Int
    }

emptyTagListList : TagListList
emptyTagListList =
    TagListList 0 Dict.empty Nothing




--Adds a tag list to a tag list list

addTagList : TagList -> TagListList -> (TagListList, Int)
addTagList addedList targetList =
    ( { targetList
        | tagLists = Dict.insert targetList.nextId addedList targetList.tagLists
        , nextId = targetList.nextId + 1
        }
    , targetList.nextId
    )

removeTagList : Int -> TagListList -> TagListList
removeTagList listId tagListList =
    let
        filterFunction key value =
            key /= listId
    in
        { tagListList | tagLists = Dict.filter filterFunction tagListList.tagLists }



startTagTextInput : Int -> TagListList -> TagListList
startTagTextInput id list =
    {list | textFieldTargetId = Just id}


cancelAddTag : TagListList -> TagListList
cancelAddTag list =
    {list | textFieldTargetId = Nothing}


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

-- Toggles the tag with tagId from the list with listId

toggleTagInTagListList : Int -> Int -> TagListList -> TagListList
toggleTagInTagListList listId tagId tagListList = 
    runOnTagList (toggleTag tagId) listId tagListList


-- Togles the specified tag list
toggleTagList : Int -> TagListList -> TagListList
toggleTagList id list =
    runOnTagList (\list -> {list | enabled = not list.enabled}) id list



-- Returns a specific TagList from a tagListList
getTagList : TagListList -> Int -> Maybe TagList
getTagList tagListList listId =
    Dict.get listId tagListList.tagLists



-- Returns a list of selected tags in a tagListList

selectedTags : TagListList -> List String
selectedTags list =
    let
        foldFunction _ value acc = 
            acc ++ if value.enabled then tagListSelectedTags value else []
    in
        Dict.foldl foldFunction [] list.tagLists


allTags : TagListList -> List String
allTags list =
    Dict.values list.tagLists
    |> List.map tagListAllTags
    |> List.concat




type alias TagListMessages msg =
    { onAddTag: (Int -> msg)
    , onRemoveList: (Int -> msg)
    , onToggleList: (Int -> msg)
    , onTagRemoveButton: (Int -> Int -> msg)
    , onTagTextClick: (Int -> Int -> msg)
    , onTagnameUnfocus: msg
    , onTagSubmit: (Int -> msg)
    , onTextChanged: (String -> msg)
    }

type SelectedTag
    = None
    | List Int
    | Single Int Int




getNthTagListId : TagListList -> Int -> Maybe Int
getNthTagListId tagListList target =
    List.Extra.getAt target <| Dict.keys tagListList.tagLists


getNthTag : TagListList -> Int -> Int -> Maybe Int
getNthTag tagListList listId target =
    let
        tagList =
            getTagList tagListList listId
    in
        case tagList of
            Just tagList ->
                List.Extra.getAt target <| Dict.keys tagList.tags
            Nothing ->
                Nothing


indicesOfTag : TagListList -> String -> List (Int, Int)
indicesOfTag tagListList tag =
    Dict.toList tagListList.tagLists
    |> List.concatMap (\(listId, list) -> tagListInidicesOfTag (\tagId -> (listId, tagId)) list tag)



tagListInidicesOfTag : (Int -> (Int, Int)) -> TagList -> String -> List (Int, Int)
tagListInidicesOfTag tupleConstructor tagList targetTag =
    Dict.toList tagList.tags
    |> List.filter (\(id, tag) -> tag.text == targetTag)
    |> List.map Tuple.first
    |> List.map tupleConstructor
