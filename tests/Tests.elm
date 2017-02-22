module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import TagListManager


tagListTest : Test
tagListTest =
    describe "Tag list management"
        [ describe "adding and removing tags"
            [ test "additionOnly" <|
                \() ->
                    let
                        manager 
                            =  TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"
                    in
                        Expect.equal ["yolo", "swag"] <| (TagListManager.tagListSelectedTags manager)
            , test "removal" <|
                \() ->
                    let
                        manager 
                            =  TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"
                            |> TagListManager.addTagToList "something"
                            |> TagListManager.removeTag 1
                    in
                        Expect.equal ["yolo", "something"] <| (TagListManager.tagListSelectedTags manager)
            ]
        , describe "TagListList management"
            [test "addition only" <|
                \() ->
                    let
                        manager1
                            = TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"

                        manager2
                            = TagListManager.emptyTagList
                            |> TagListManager.addTagToList "foo"
                            |> TagListManager.addTagToList "bar"


                        listList = TagListManager.emptyTagListList
                            |> TagListManager.addTagList manager1
                            |> TagListManager.addTagList manager2
                    in
                        Expect.equal ["yolo", "swag", "foo", "bar"] <| (TagListManager.selectedTags listList)
                ]
        , describe "TagListList management"
            [test "removal" <|
                \() ->
                    let
                        manager1
                            = TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"

                        manager2
                            = TagListManager.emptyTagList
                            |> TagListManager.addTagToList "foo"
                            |> TagListManager.addTagToList "bar"

                        manager3
                            = TagListManager.emptyTagList
                            |> TagListManager.addTagToList "hello"
                            |> TagListManager.addTagToList "world"


                        listList = TagListManager.emptyTagListList
                            |> TagListManager.addTagList manager1
                            |> TagListManager.addTagList manager2
                            |> TagListManager.addTagList manager3
                            |> TagListManager.removeTagFromTagListList 2 1
                            |> TagListManager.removeTagList 1
                    in
                        Expect.equal ["yolo", "swag", "hello"] <| (TagListManager.selectedTags listList)
                ]
        ]

