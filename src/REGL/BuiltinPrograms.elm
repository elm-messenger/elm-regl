module REGL.BuiltinPrograms exposing
    ( clear, triangle, quad, texture, rectTexture
    , textbox, circle, centeredTexture, polyPrim, poly
    , lines, linestrip, lineloop, functionCurve
    , toRgbaList, Primitive(..), primitiveToValue
    , saveAsTexture
    )

{-|


# Builtin Programs


## Builtin Commands

@docs clear, triangle, quad, texture, rectTexture
@docs textbox, circle, centeredTexture, polyPrim, poly
@docs lines, linestrip, lineloop, functionCurve


## Utils

@docs toRgbaList, Primitive, primitiveToValue


## Advanced

@docs saveAsTexture

-}

import Color exposing (Color)
import Json.Encode as Encode exposing (Value)
import REGL.Common exposing (Renderable, genProg)


{-| OpenGL primitive types
-}
type Primitive
    = Points
    | Lines
    | LineLoop
    | LineStrip
    | Triangles
    | TriangleStrip
    | TriangleFan


{-| Convert a primitive to a value.
-}
primitiveToValue : Primitive -> Value
primitiveToValue p =
    case p of
        Points ->
            Encode.string "points"

        Lines ->
            Encode.string "lines"

        LineLoop ->
            Encode.string "line loop"

        LineStrip ->
            Encode.string "line strip"

        Triangles ->
            Encode.string "triangles"

        TriangleStrip ->
            Encode.string "triangle strip"

        TriangleFan ->
            Encode.string "triangle fan"


{-| Convert a color to a list of RGBA values.
-}
toRgbaList : Color -> List Float
toRgbaList c =
    let
        rgba =
            Color.toRgba c
    in
    [ rgba.red, rgba.green, rgba.blue, rgba.alpha ]


{-| Clear the canvas with a color.
-}
clear : Color -> Renderable
clear color =
    genProg
        (Encode.object
            [ ( "cmd", Encode.int 1 )
            , ( "name", Encode.string "clear" )
            , ( "args"
              , Encode.object
                    [ ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "depth", Encode.float 1 )
                    ]
              )
            ]
        )


{-| Render a triangle with three vertices and color.
-}
triangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
triangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "triangle" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3 ] )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    ]
              )
            ]


{-| Render a quad with three vertices and color.
-}
quad : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
quad ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "quad" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    ]
              )
            ]


{-| Render a poly with vertices and color.
-}
poly : List ( Float, Float ) -> Color -> Renderable
poly xs color =
    let
        pos =
            List.concatMap (\( x, y ) -> [ x, y ]) xs

        elem =
            List.concatMap (\x -> [ 0, toFloat x, toFloat x + 1 ]) (List.range 1 (List.length xs - 2))
    in
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "poly" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float pos )
                    , ( "elem", Encode.list Encode.float elem )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    ]
              )
            ]


{-| Render lines with vertices and color.
-}
lines : List ( ( Float, Float ), ( Float, Float ) ) -> Color -> Renderable
lines xs color =
    let
        pos =
            List.concatMap (\( ( x1, y1 ), ( x2, y2 ) ) -> [ x1, y1, x2, y2 ]) xs

        elem =
            List.map toFloat <| List.range 0 (2 * List.length xs - 1)
    in
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "poly" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float pos )
                    , ( "elem", Encode.list Encode.float elem )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "prim", primitiveToValue Lines )
                    ]
              )
            ]


{-| Render line strip with vertices and color.
-}
linestrip : List ( Float, Float ) -> Color -> Renderable
linestrip xs color =
    let
        pos =
            List.concatMap (\( x, y ) -> [ x, y ]) xs

        elem =
            List.map toFloat <| List.range 0 (List.length xs - 1)
    in
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "poly" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float pos )
                    , ( "elem", Encode.list Encode.float elem )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "prim", primitiveToValue LineStrip )
                    ]
              )
            ]


{-| Render a line loop with vertices and color.
-}
lineloop : List ( Float, Float ) -> Color -> Renderable
lineloop xs color =
    let
        pos =
            List.concatMap (\( x, y ) -> [ x, y ]) xs

        elem =
            List.map toFloat <| List.range 0 (List.length xs - 1)
    in
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "poly" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float pos )
                    , ( "elem", Encode.list Encode.float elem )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "prim", primitiveToValue LineLoop )
                    ]
              )
            ]


{-| Render a function curve with a function, offset, range, samples, and color.
-}
functionCurve : (Float -> Float) -> ( Float, Float ) -> ( Float, Float ) -> Float -> Color -> Renderable
functionCurve f ( x, y ) ( left, right ) freq color =
    let
        samples =
            ceiling (freq * (right - left))

        xs =
            List.map (\u -> (toFloat u / toFloat samples) * (right - left) + left) <| List.range 0 samples

        xys =
            List.map (\posx -> ( posx + x, f posx + y )) xs
    in
    linestrip xys color


{-| Render a poly with vertices, element array, primitives and color.
-}
polyPrim : List ( Float, Float ) -> List Float -> Color -> Primitive -> Renderable
polyPrim xs elem color prim =
    let
        pos =
            List.concatMap (\( x, y ) -> [ x, y ]) xs
    in
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "poly" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float pos )
                    , ( "elem", Encode.list Encode.float elem )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    , ( "prim", primitiveToValue prim )
                    ]
              )
            ]


{-| Render a circle with center, radius and color.
-}
circle : ( Float, Float ) -> Float -> Color -> Renderable
circle ( x1, y1 ) r color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "circle" )
            , ( "args"
              , Encode.object
                    [ ( "center", Encode.list Encode.float [ x1, y1 ] )
                    , ( "radius", Encode.float r )
                    , ( "color", Encode.list Encode.float (toRgbaList color) )
                    ]
              )
            ]


{-| Render a texture with 4 points: left-bottom, right-bottom, right-top and left-top.
-}
texture : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
texture ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) name =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "texture" )
            , ( "args"
              , Encode.object
                    [ ( "texture", Encode.string name )
                    , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
                    ]
              )
            ]


{-| Render a texture with left-bottom coordinates and size.
-}
rectTexture : ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
rectTexture ( x1, y1 ) ( w, h ) name =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "texture" )
            , ( "args"
              , Encode.object
                    [ ( "texture", Encode.string name )
                    , ( "pos", Encode.list Encode.float [ x1, y1, x1 + w, y1, x1 + w, y1 + h, x1, y1 + h ] )
                    ]
              )
            ]


{-| Render a texture with center, size and angle for rotation.
-}
centeredTexture : ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
centeredTexture ( x, y ) ( w, h ) angle name =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "centeredTexture" )
            , ( "args"
              , Encode.object
                    [ ( "texture", Encode.string name )
                    , ( "center", Encode.list Encode.float [ x, y ] )
                    , ( "size", Encode.list Encode.float [ w, h ] )
                    , ( "angle", Encode.float angle )
                    ]
              )
            ]


{-| Render a textbox.
-}
textbox : ( Float, Float ) -> Float -> String -> String -> Color -> Renderable
textbox ( x, y ) size text font color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "textbox" )
            , ( "args"
              , Encode.object
                    [ ( "text", Encode.string text )
                    , ( "size", Encode.float size )
                    , ( "offset", Encode.list Encode.float [ x, y ] )
                    , ( "font", Encode.string font )
                    , ( "color", Encode.list Encode.float <| toRgbaList color )
                    ]
              )
            ]


{-| Full TextBox options.
-}
type alias TextBoxOption =
    { font : String
    , text : String
    , size : Float
    , color : Color
    , wordBreak : Bool
    , width : Maybe Float
    , lineHeight : Maybe Float
    , spaceWidth : Maybe Float
    , align : Maybe String
    }


{-| Render a textbox with more options.
-}
textboxPro : ( Float, Float ) -> TextBoxOption -> Renderable
textboxPro ( x, y ) opt =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "textbox" )
            , ( "args"
              , Encode.object
                    [ ( "text", Encode.string opt.text )
                    , ( "size", Encode.float opt.size )
                    , ( "offset", Encode.list Encode.float [ x, y ] )
                    , ( "font", Encode.string opt.font )
                    , ( "color", Encode.list Encode.float <| toRgbaList opt.color )
                    , ( "wordBreak", Encode.bool opt.wordBreak )
                    , ( "width", Maybe.withDefault (Encode.null) <| Maybe.map Encode.float opt.width )
                    , ( "lineHeight", Maybe.withDefault (Encode.null) <| Maybe.map Encode.float opt.lineHeight )
                    , ( "spaceWidth", Maybe.withDefault (Encode.null) <| Maybe.map Encode.float opt.spaceWidth )
                    , ( "align", Maybe.withDefault (Encode.null) <| Maybe.map Encode.float opt.align )
                    ]
              )
            ]


{-| Save the current FBO as a texture.

This is only an experimental feature and should be used with caution.

-}
saveAsTexture : String -> Renderable
saveAsTexture text =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 4 )
            , ( "name", Encode.string text )
            ]
