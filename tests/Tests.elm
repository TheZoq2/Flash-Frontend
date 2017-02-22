module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Tags


tagListTest : Test
tagListTest =
    describe "Tag list management"
        [ describe "adding and removing tags"
            [ test "additionOnly" <|
                \() ->
                    let
                        manager 
                            =  Tags.emptyTagList
                            |> Tags.addTagToList "yolo"
                            |> Tags.addTagToList "swag"
                    in
                        Expect.equal ["yolo", "swag"] <| (Tags.tagListSelectedTags manager)
            , test "removal" <|
                \() ->
                    let
                        manager 
                            =  Tags.emptyTagList
                            |> Tags.addTagToList "yolo"
                            |> Tags.addTagToList "swag"
                            |> Tags.addTagToList "something"
                            |> Tags.removeTag 1
                    in
                        Expect.equal ["yolo", "something"] <| (Tags.tagListSelectedTags manager)
            ]
        , describe "TagListList management"
            [test "addition only" <|
                \() ->
                    let
                        manager1
                            = Tags.emptyTagList
                            |> Tags.addTagToList "yolo"
                            |> Tags.addTagToList "swag"

                        manager2
                            = Tags.emptyTagList
                            |> Tags.addTagToList "foo"
                            |> Tags.addTagToList "bar"


                        listList = Tags.emptyTagListList
                            |> Tags.addTagList manager1
                            |> Tags.addTagList manager2
                    in
                        Expect.equal ["yolo", "swag", "foo", "bar"] <| (Tags.selectedTags listList)
                ]
        , describe "TagListList management"
            [test "removal" <|
                \() ->
                    let
                        manager1
                            = Tags.emptyTagList
                            |> Tags.addTagToList "yolo"
                            |> Tags.addTagToList "swag"

                        manager2
                            = Tags.emptyTagList
                            |> Tags.addTagToList "foo"
                            |> Tags.addTagToList "bar"

                        manager3
                            = Tags.emptyTagList
                            |> Tags.addTagToList "hello"
                            |> Tags.addTagToList "world"


                        listList = Tags.emptyTagListList
                            |> Tags.addTagList manager1
                            |> Tags.addTagList manager2
                            |> Tags.addTagList manager3
                            |> Tags.removeTagFromTagListList 2 1
                            |> Tags.removeTagList 1
                    in
                        Expect.equal ["yolo", "swag", "hello"] <| (Tags.selectedTags listList)
                ]
        ]

