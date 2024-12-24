module REGL exposing (..)

import Html exposing (Attribute, Html, canvas)
import Html.Attributes exposing (height, id, width)
import Html.Keyed as Keyed
import Json.Encode as Encode
import REGL.API exposing (clearREGL)
import REGL.Common exposing (Color, Renderable(..))


group : List Renderable -> Renderable
group renderables =
    GroupRenderable renderables


genProg : Encode.Value -> Renderable
genProg =
    AtomicRenderable


empty : Renderable
empty =
    genProg Encode.null


render : Renderable -> Encode.Value
render renderable =
    case renderable of
        AtomicRenderable value ->
            value

        GroupRenderable renderables ->
            Encode.list identity (List.map render renderables)


clear : Color -> Float -> Renderable
clear color depth =
    genProg (Encode.object (clearREGL color depth))


toHtmlWith :
    { width : Int
    , height : Int
    }
    -> List (Attribute msg)
    -> Html msg
toHtmlWith options attrs =
    Keyed.node "elm-regl"
        attrs
        [ ( "__canvas", canvas [ height options.height, width options.width, id "elm-regl-canvas" ] [] ) ]
