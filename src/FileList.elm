module FileList exposing
    ( FileList
    , new
    , jump
    , decodeNewFileList
    , fileListUrl
    )

import Json.Decode exposing (..)

type alias FileList =
    { listId: Int
    , fileIndex: Int
    , length: Int
    }

new : Int -> Int -> FileList
new id length =
    FileList id 0 length



-- Attempts to set the index of the list to the current index + jump
-- If this the new index is outside of 0 < index < length, the original
-- list is returned
jump : Int -> FileList -> FileList
jump amount list =
    let
        newIndex = list.fileIndex + amount
    in
        if newIndex < list.length && newIndex >= 0 then
            { list | fileIndex = newIndex }
        else
            list



type alias FileListResponse =
    { id: Int
    , length: Int
    }

decodeNewFileList : Json.Decode.Decoder FileListResponse
decodeNewFileList =
    Json.Decode.map2 FileListResponse
        (field "id" Json.Decode.int)
        (field "length" Json.Decode.int)


fileListUrl :List (String, String) -> String -> Int -> Int ->  String
fileListUrl additionalVariables action listId fileIndex =
    let
        baseUrl = "list?"

        variables = [ ("action", action)
                    , ("list_id", toString listId)
                    , ("index", toString fileIndex)
                    ]
                    ++ additionalVariables

        variableStrings = List.intersperse "&"
                        <| List.map (\(name, value) -> name ++ "=" ++ value) variables
    in
        baseUrl ++ List.foldr (++) "" variableStrings


