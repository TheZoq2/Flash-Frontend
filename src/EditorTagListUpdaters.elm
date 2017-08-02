module EditorTagListUpdaters exposing
    ( addTagList
    , toggleTagList
    , toggleTag
    , removeTag
    , removeTagList
    , startTagAddition
    , cancelTagCreation
    )

import EditorModel exposing 
    ( Model
    , FileData
    , KeyReceiver(..)
    )
import EditorMsg exposing
    ( Msg (..)
    )

import Tags exposing (TagList, TagListList)
import ImageViewer
import FileList exposing (fileListDecoder, fileListFileUrl, fileListListUrl)
import Touch

import Vec exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task
import Window
import Keyboard
import Char
import Dom
import List.Extra
import UrlParser
import Navigation
import UrlParser
import UrlParser exposing ((</>))
import Math.Vector2 exposing (Vec2, vec2)


addTagList : Model -> (Model, Cmd Msg)
addTagList model =
    let
        (newTags, _) = Tags.addTagList Tags.emptyTagList model.tags
    in
        ( {model | tags = newTags }, Cmd.none)



startTagAddition : Model -> Int -> (Model, Cmd Msg)
startTagAddition model tagListId =
    let
        newTags = Tags.startTagTextInput tagListId model.tags
        cmd = Dom.focus "tag_input_field" |> Task.attempt FocusResult
    in
        ({model | tags = newTags, keyReceiver = TagField tagListId}, cmd)


removeTagList : Model -> Int -> (Model, Cmd Msg)
removeTagList model id =
    let
        newTags =
            Tags.removeTagList id model.tags

        newReceiver =
            case model.keyReceiver of
                TagList _ ->
                    EditorModel.TagListList
                TagField _ ->
                    EditorModel.TagListList
                old -> old
    in
        ({model | tags = newTags, keyReceiver = newReceiver}, Cmd.none)



toggleTag : Model -> Int -> Int -> (Model, Cmd Msg)
toggleTag model listId tagId =
    let
        newTags = Tags.toggleTagInTagListList listId tagId model.tags
    in
        ({model | tags = newTags}, Cmd.none)



removeTag : Model -> Int -> Int -> (Model, Cmd Msg)
removeTag model listId tagId =
    let
        newTags =
            Tags.removeTagFromTagListList listId tagId model.tags

        newReceiver =
            case model.keyReceiver of
                Tag listId _ ->
                    EditorModel.TagList listId
                old -> old
    in
        ({model | tags = newTags, keyReceiver = newReceiver}, Cmd.none)




toggleTagList : Model -> Int -> (Model, Cmd Msg)
toggleTagList model id =
    let
        newTags = Tags.toggleTagList id model.tags
    in
        ({model | tags = newTags}, Cmd.none)



cancelTagCreation : Model -> Model
cancelTagCreation model =
    let
        newReceiver = case model.keyReceiver of
            TagField id ->
                EditorModel.TagList id
            _ ->
                None
    in
        {model | tagTextfieldContent = Nothing
               , tags = Tags.cancelAddTag model.tags
               , keyReceiver = newReceiver
               }

