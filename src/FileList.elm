module FileList exposing
    ( FileList
    , FileListResponse
    , FileListSource(Folder, Search)
    , new
    , newWithSelected
    , jump
    , fileListDecoder
    , fileListFileUrl
    , fileListListUrl
    , requestFileListListing
    )

import Json.Decode exposing (..)
import Http

type alias FileList =
    { listId: Int
    , fileIndex: Int
    , length: Int
    }

new : Int -> Int -> FileList
new id length =
    FileList id 0 length

newWithSelected : Int -> Int -> Int -> FileList
newWithSelected selected id length =
    let
        fileIndex =
            if selected >= length then
                length - 1
            else if selected < 0 then
                0
            else
                selected
    in
        FileList id fileIndex length



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



type FileListSource
    = Folder String
    | Search

type alias FileListResponse =
    { id: Int
    , length: Int
    , source: FileListSource
    }

fileListSourceDecoder : Json.Decode.Decoder FileListSource
fileListSourceDecoder =
    let
        folderDecoder : Json.Decode.Decoder FileListSource
        folderDecoder =
            Json.Decode.map Folder
                (field "Folder" Json.Decode.string)

        searchDecoder : Json.Decode.Decoder FileListSource
        searchDecoder =
            Json.Decode.string 
                |> Json.Decode.andThen(\str ->
                        case str of 
                            "Search" -> Json.Decode.succeed Search
                            somethingElse ->
                                Json.Decode.fail <| somethingElse ++ "is not a file list source"
                    )
                
    in
        Json.Decode.oneOf
            [ folderDecoder
            , searchDecoder
            ]


fileListDecoder : Json.Decode.Decoder FileListResponse
fileListDecoder =
    Json.Decode.map3 FileListResponse
        (field "id" Json.Decode.int)
        (field "length" Json.Decode.int)
        (field "source" fileListSourceDecoder)


fileListUrl : List (String, String) -> String -> String
fileListUrl additionalVariables action =
    let
        baseUrl = "list?"

        variables = [ ("action", action)
                    ]
                    ++ additionalVariables

        variableStrings = List.intersperse "&"
                        <| List.map (\(name, value) -> name ++ "=" ++ value) variables
    in
        baseUrl ++ List.foldr (++) "" variableStrings

fileListListUrl : List(String, String) -> String -> Int -> String
fileListListUrl additionalVariables action listId =
    let
        variables =
            [ ("list_id", toString listId)
            ]
            ++ additionalVariables
    in
        fileListUrl variables action

fileListFileUrl : List (String, String) -> String -> Int -> Int ->  String
fileListFileUrl additionalVariables action listId fileIndex =
    fileListListUrl ([ ("index", toString fileIndex) ] ++ additionalVariables) action listId


fileListListingDecoder : Json.Decode.Decoder (List FileListResponse)
fileListListingDecoder =
    Json.Decode.list fileListDecoder


checkHttpAttempt : (a -> msg) -> (Http.Error -> msg)-> Result Http.Error a -> msg
checkHttpAttempt func errFunc res=
    case res of
        Ok val ->
            func val
        Err e ->
            errFunc e


requestFileListListing : (List FileListResponse -> msg) -> (Http.Error -> msg) -> Cmd msg
requestFileListListing onSuccess onError =
    let
        url = fileListUrl [] "lists"
    in
        Http.send
            (checkHttpAttempt onSuccess onError)
            (Http.get url fileListListingDecoder)

