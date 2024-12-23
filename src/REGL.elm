module REGL exposing (..)

import Html exposing (Attribute, Html, canvas)
import Html.Attributes exposing (height, id, width)
import Html.Keyed as Keyed
import Json.Encode as Encode
import REGL.API exposing (appendArgs, clearREGL, toRgbaList, triangleProgram)
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


triangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
triangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg <|
        appendArgs
            [ ( "x", Encode.list Encode.float [ x1, y1 ] )
            , ( "y", Encode.list Encode.float [ x2, y2 ] )
            , ( "z", Encode.list Encode.float [ x3, y3 ] )
            , ( "color", Encode.list Encode.float (toRgbaList color) )
            ]
            triangleProgram


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
