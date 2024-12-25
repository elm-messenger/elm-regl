module REGL exposing
    ( Renderable, genProg, group, empty, render
    , clear, triangle, simpTexture
    , REGLConfig, TimeInterval(..), encodeConfig
    , toHtmlWith
    )

{-|


# REGL


## Basics

@docs Renderable, genProg, group, empty, render


## Builtin Commands

@docs clear, triangle, simpTexture


## User Configuration

@docs REGLConfig, TimeInterval, encodeConfig

-}

import Color exposing (Color)
import Html exposing (Attribute, Html, canvas)
import Html.Attributes exposing (height, id, width)
import Html.Keyed as Keyed
import Json.Encode as Encode exposing (Value)
import REGL.Common as C


{-| A renderable object that can be rendered by REGL.
-}
type alias Renderable =
    C.Renderable


{-| Generate a renderable object from an object.
-}
genProg : Value -> Renderable
genProg =
    C.genProg


{-| Group a list of renderables into a single renderable.
-}
group : List Renderable -> Renderable
group =
    C.group


{-| An empty renderable object.
-}
empty : Renderable
empty =
    genProg Encode.null


{-| Render a renderable object.
-}
render : Renderable -> Value
render =
    C.render


toRgbaList : Color -> List Float
toRgbaList c =
    let
        rgba =
            Color.toRgba c
    in
    [ rgba.red, rgba.green, rgba.blue, rgba.alpha ]


{-| Clear the canvas with a color and a depth value.
-}
clear : Color -> Float -> Renderable
clear color depth =
    genProg
        (Encode.object
            [ ( "cmd", Encode.int 1 )
            , ( "name", Encode.string "clear" )
            , ( "args"
              , Encode.object
                    [ ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "depth", Encode.float depth )
                    ]
              )
            ]
        )


{-| Render a triangle with three vertices and a color.
-}
triangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
triangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "program", Encode.string "triangle" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3 ] )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    ]
              )
            ]


simpTexture : ( Float, Float ) -> String -> Renderable
simpTexture ( x1, y1 ) name =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "program", Encode.string "simpTexture" )
            , ( "args"
              , Encode.object
                    [ ( "texture", Encode.string name )
                    , ( "offset", Encode.list Encode.float [ x1, y1 ] )
                    ]
              )
            ]


{-| Create the canvas HTML element.
-}
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


{-| A time interval for the REGL configuration.
-}
type TimeInterval
    = AnimationFrame
    | Millisecond Float


{-| The user configuration for REGL.
-}
type alias REGLConfig =
    { timeInterval : TimeInterval
    }


{-| Encode the REGL configuration to an object.
-}
encodeConfig : REGLConfig -> Value
encodeConfig config =
    let
        interval =
            case config.timeInterval of
                AnimationFrame ->
                    -1

                Millisecond ms ->
                    ms
    in
    Encode.object
        [ ( "interval", Encode.float interval )
        ]
