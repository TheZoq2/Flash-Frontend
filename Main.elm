import TagListManager
import Style

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode 
import Json.Encode
import Http
import Task






--Model

type alias Model = 
    {
        tagListManager: TagListManager.Model,
        
        tagNames: List String,

        currentImages: List String,

        networkError: String
    }

init : (Model, Cmd Msg)
init =
    let
        tagListManager = TagListManager.init
        
        tagNames = TagListManager.getTagList tagListManager
    in
        (
            {
                tagListManager = TagListManager.init,

                tagNames = TagListManager.getTagList tagListManager,

                currentImages = [],

                networkError = ""
            },
            Cmd.none
        )





--Update

type Msg 
    = TagListMsg TagListManager.Msg
    | ListingFail Http.Error
    | AlbumListFetched (List String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TagListMsg tagMsg ->
            let 
                newModel = {model | tagListManager = TagListManager.update tagMsg model.tagListManager}

                newTagNames = TagListManager.getTagList newModel.tagListManager

                --If the list of tags has changed since the last time we checked
                cmd = if newTagNames == model.tagNames then
                    Cmd.none
                else
                    --Request new images
                    getImagesWithTags newTagNames

                --Update the model with the new list
                finalModel = {newModel | tagNames = newTagNames}
            in
                (finalModel, cmd)

        ListingFail err ->
            ({model | networkError = toString err}, Cmd.none)

        AlbumListFetched result ->
            let
                _ = Debug.log "Response " result
            in
                ({model | currentImages = result}, Cmd.none)



--Makes a HTTP request to the server requesting a list of all the images
--with the specified tags
getImagesWithTags : List String -> Cmd Msg
getImagesWithTags tags =
    let 
        tagsJson = List.map Json.Encode.string tags
        
        --TODO: Make surre X-Origin requests are allowed
        url = "http://localhost:3000/album?tags=" ++ toString tagsJson
    in
        Task.perform ListingFail AlbumListFetched (Http.get decodeAlbumList url)




decodeAlbumList : Json.Decode.Decoder (List String)
decodeAlbumList =
    Json.Decode.at [] (Json.Decode.list Json.Decode.string)







--View

view : Model -> Html Msg
view model =
    div [Style.toStyle Style.albumContainer]
    ([
        Html.App.map TagListMsg (TagListManager.view model.tagListManager),

        p [] [text model.networkError]
    ] ++ generateImageViews model.currentImages)


generateImageViews : List String -> List (Html Msg)
generateImageViews images =
    List.map (\image -> img [width 200, height 200, src ("http://localhost:3000/album/image/" ++ image)] []) images



--Subscriptions

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none



--Main

main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
