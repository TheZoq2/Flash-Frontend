module Urls exposing 
    ( fileListListingUrl
    , fileListGetThumbnailUrl
    , fileListGetFileUrl
    , fileListGetDataUrl
    , fileListSaveUrl
    , fileListListInfoUrl
    , fileListLastSavedIndexUrl
    , searchUrl
    )

import Json.Encode


-- Helper functions for creating urls

rawUrlWithAction : List String -> String -> List (String, String) -> String
rawUrlWithAction url action additionalParameters =
    rawUrl url ([("action", action)] ++ additionalParameters)


rawUrl : List String -> List (String, String) -> String
rawUrl url queryParameters =
    let
        urlString = List.intersperse "/" url |> List.foldr (++) ""

        variableString =
            List.foldr (++) ""
                <| List.intersperse "&"
                <| List.map (\(name, value) -> name ++ "=" ++ value) queryParameters
    in
        urlString ++ "?" ++ variableString








-- Urls for the /list endpoint


fileListListingUrl : String
fileListListingUrl =
    rawUrlWithAction
        ["list"] 
        "lists"
        []


fileListGetThumbnailUrl : Int -> Int -> String
fileListGetThumbnailUrl listId fileIndex =
    rawUrlWithAction
        ["list"]
        "get_thumbnail"
        (fileListFileParameters listId fileIndex)

fileListGetFileUrl : Int -> Int -> String
fileListGetFileUrl listId fileIndex =
    rawUrlWithAction
        ["list"]
        "get_file"
        (fileListFileParameters listId fileIndex)


fileListGetDataUrl : Int -> Int -> String
fileListGetDataUrl listId fileIndex =
    rawUrlWithAction
        ["list"]
        "get_data"
        (fileListFileParameters listId fileIndex)


fileListSaveUrl : Int -> Int -> List String -> String
fileListSaveUrl listId fileIndex tags =
    let
        tagsJson =
            Json.Encode.encode 0 <| Json.Encode.list Json.Encode.string tags
    in
        rawUrlWithAction
            ["list"]
            "save"
            ((fileListFileParameters listId fileIndex) ++ [("tags", tagsJson)])

fileListListInfoUrl : Int -> String
fileListListInfoUrl id =
    rawUrlWithAction
        ["list"]
        "list_info"
        [("list_id", String.fromInt id)]


fileListLastSavedIndexUrl : Int -> String
fileListLastSavedIndexUrl id =
    rawUrlWithAction
        ["list"]
        "list_last_saved_index"
        [("list_id", String.fromInt id)]










-- Urls for the search endpoint

searchUrl : String -> String
searchUrl query =
    rawUrl
        ["search"]
        [("query", query)]






-- Helper function for common parameters

fileListFileParameters : Int -> Int -> List (String, String)
fileListFileParameters listId fileIndex =
    [("list_id", String.fromInt listId), ("index", String.fromInt fileIndex)]

