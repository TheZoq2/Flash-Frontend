module Requests exposing (checkHttpAttempt, requestSubdirectories)

import AlbumMsg exposing (Msg(..))

import Http
import Json.Decode

checkHttpAttempt : (a -> Msg) -> Result Http.Error a -> Msg
checkHttpAttempt func res=
    case res of
        Ok val ->
            func val
        Err e ->
            NetworkError e


requestSubdirectories : Cmd Msg
requestSubdirectories =
    Http.send
        (checkHttpAttempt <| AvailableFoldersReceived)
        (Http.get ("subdirectories") <| Json.Decode.list Json.Decode.string)


