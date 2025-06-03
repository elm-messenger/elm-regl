module REGL.BuiltinPrograms exposing
    ( clear
    , triangle, quad, rectCentered, rect, circle, polyPrim, poly
    , textbox, textboxMF, textboxPro, TextBoxOption, defaultTextBoxOption
    , texture, rectTexture, textureCropped, rectTextureCropped, centeredTexture, centeredTextureCropped
    , centeredTextureWithAlpha, rectTextureCroppedWithAlpha, rectTextureWithAlpha, textureCroppedWithAlpha, textureWithAlpha, centeredTextureCroppedWithAlpha
    , lines, linestrip, lineloop, functionCurve
    , toRgbaList, Primitive(..), primitiveToValue
    , saveAsTexture
    )

{-|


# Builtin Programs


## Builtin Commands

@docs clear
@docs triangle, quad, rectCentered, rect, circle, polyPrim, poly
@docs textbox, textboxMF, textboxPro, TextBoxOption, defaultTextBoxOption
@docs texture, rectTexture, textureCropped, rectTextureCropped, centeredTexture, centeredTextureCropped
@docs centeredTextureWithAlpha, rectTextureCroppedWithAlpha, rectTextureWithAlpha, textureCroppedWithAlpha, textureWithAlpha, centeredTextureCroppedWithAlpha
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
        [ ( "_c", Encode.int 1 )
        , ( "_n", Encode.string "clear" )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        , ( "depth", Encode.float 1 )
        ]


{-| Render a triangle with three vertices and color.
-}
triangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
triangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "triangle" )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3 ] )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        ]


{-| Render a quad with four vertices and color.
-}
quad : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
quad ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "quad" )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        ]


{-| Render a rectangle with center, size, angle and color.
-}
rectCentered : ( Float, Float ) -> ( Float, Float ) -> Float -> Color -> Renderable
rectCentered ( x, y ) ( w, h ) angle color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "rect" )
        , ( "posize", Encode.list Encode.float [ x, y, w, h ] )
        , ( "angle", Encode.float angle )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        ]


{-| Render a rectangle with left-top coordinate, size and color.
-}
rect : ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
rect ( x, y ) ( w, h ) color =
    rectCentered ( x + w / 2, y + h / 2 ) ( w, h ) 0 color


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
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "poly" )
        , ( "pos", Encode.list Encode.float pos )
        , ( "elem", Encode.list Encode.float elem )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
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
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "poly" )
        , ( "pos", Encode.list Encode.float pos )
        , ( "elem", Encode.list Encode.float elem )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        , ( "prim", primitiveToValue Lines )
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
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "poly" )
        , ( "pos", Encode.list Encode.float pos )
        , ( "elem", Encode.list Encode.float elem )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        , ( "prim", primitiveToValue LineStrip )
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
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "poly" )
        , ( "pos", Encode.list Encode.float pos )
        , ( "elem", Encode.list Encode.float elem )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        , ( "prim", primitiveToValue LineLoop )
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
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "poly" )
        , ( "pos", Encode.list Encode.float pos )
        , ( "elem", Encode.list Encode.float elem )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        , ( "prim", primitiveToValue prim )
        ]


{-| Render a circle with center, radius and color.
-}
circle : ( Float, Float ) -> Float -> Color -> Renderable
circle ( x1, y1 ) r color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "circle" )
        , ( "center", Encode.list Encode.float [ x1, y1 ] )
        , ( "radius", Encode.float r )
        , ( "color", Encode.list Encode.float (toRgbaList color) )
        ]


{-| Render a texture with 4 points: left-top, right-top, right-bottom and left-bottom.
-}
texture : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
texture ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "texture" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
        ]


{-| Render a texture with 4 points: left-top, right-top, right-bottom and left-bottom, along with 4 cropped points in texture coordinate.
-}
textureCropped : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
textureCropped ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) ( cx1, cy1 ) ( cx2, cy2 ) ( cx3, cy3 ) ( cx4, cy4 ) name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textureCropped" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
        , ( "texc", Encode.list Encode.float [ cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4 ] )
        ]


{-| Render a texture with left-top coordinates and size.
-}
rectTexture : ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
rectTexture ( x, y ) ( w, h ) =
    centeredTexture ( x + w / 2, y + h / 2 ) ( w, h ) 0


{-| Render a texture with left-top coordinates and size, along with a crop rectangle in texture coordinate.
-}
rectTextureCropped : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
rectTextureCropped ( x, y ) ( w, h ) ( cx, cy ) ( cw, ch ) name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textureCropped" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x, y, x + w, y, x + w, y + h, x, y + h ] )
        , ( "texc", Encode.list Encode.float [ cx, 1 - cy, cx + cw, 1 - cy, cx + cw, 1 - cy - ch, cx, 1 - cy - ch ] )
        ]


{-| Render a texture with center, size and angle for rotation.
-}
centeredTexture : ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
centeredTexture ( x, y ) ( w, h ) angle name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "centeredTexture" )
        , ( "texture", Encode.string name )
        , ( "posize", Encode.list Encode.float [ x, y, w, h ] )
        , ( "angle", Encode.float angle )
        ]


{-| Render a texture with center, size and angle for rotation.
-}
centeredTextureCropped : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> String -> Renderable
centeredTextureCropped ( x, y ) ( w, h ) angle ( cx, cy ) ( cw, ch ) name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "centeredCroppedTexture" )
        , ( "texture", Encode.string name )
        , ( "posize", Encode.list Encode.float [ x, y, w, h ] )
        , ( "angle", Encode.float angle )
        , ( "texc", Encode.list Encode.float [ cx, cy, cw, ch ] )
        ]


{-| Render a texture with 4 points: left-top, right-top, right-bottom and left-bottom.
-}
textureWithAlpha : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
textureWithAlpha ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) alpha name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "texture" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
        , ( "alpha", Encode.float alpha )
        ]


{-| Render a texture with 4 points: left-top, right-top, right-bottom and left-bottom, along with 4 cropped points in texture coordinate.
-}
textureCroppedWithAlpha : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
textureCroppedWithAlpha ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ( x4, y4 ) ( cx1, cy1 ) ( cx2, cy2 ) ( cx3, cy3 ) ( cx4, cy4 ) alpha name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textureCropped" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3, x4, y4 ] )
        , ( "texc", Encode.list Encode.float [ cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4 ] )
        , ( "alpha", Encode.float alpha )
        ]


{-| Render a texture with left-top coordinates and size.
-}
rectTextureWithAlpha : ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
rectTextureWithAlpha ( x, y ) ( w, h ) =
    centeredTextureWithAlpha ( x + w / 2, y + h / 2 ) ( w, h ) 0


{-| Render a texture with left-top coordinates and size, along with a crop rectangle in texture coordinate.
-}
rectTextureCroppedWithAlpha : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
rectTextureCroppedWithAlpha ( x, y ) ( w, h ) ( cx, cy ) ( cw, ch ) alpha name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textureCropped" )
        , ( "texture", Encode.string name )
        , ( "pos", Encode.list Encode.float [ x, y, x + w, y, x + w, y + h, x, y + h ] )
        , ( "texc", Encode.list Encode.float [ cx, 1 - cy, cx + cw, 1 - cy, cx + cw, 1 - cy - ch, cx, 1 - cy - ch ] )
        , ( "alpha", Encode.float alpha )
        ]


{-| Render a texture with center, size and angle for rotation.
-}
centeredTextureWithAlpha : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> String -> Renderable
centeredTextureWithAlpha ( x, y ) ( w, h ) angle alpha name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "centeredTexture" )
        , ( "texture", Encode.string name )
        , ( "posize", Encode.list Encode.float [ x, y, w, h ] )
        , ( "angle", Encode.float angle )
        , ( "alpha", Encode.float alpha )
        ]


{-| Render a texture with center, size and angle for rotation.
-}
centeredTextureCroppedWithAlpha : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> Float -> String -> Renderable
centeredTextureCroppedWithAlpha ( x, y ) ( w, h ) angle ( cx, cy ) ( cw, ch ) alpha name =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "centeredCroppedTexture" )
        , ( "texture", Encode.string name )
        , ( "posize", Encode.list Encode.float [ x, y, w, h ] )
        , ( "angle", Encode.float angle )
        , ( "texc", Encode.list Encode.float [ cx, cy, cw, ch ] )
        , ( "alpha", Encode.float alpha )
        ]


{-| Render a textbox.
-}
textbox : ( Float, Float ) -> Float -> String -> String -> Color -> Renderable
textbox ( x, y ) size text font color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textbox" )
        , ( "text", Encode.string text )
        , ( "size", Encode.float size )
        , ( "offset", Encode.list Encode.float [ x, y ] )
        , ( "font", Encode.string font )
        , ( "color", Encode.list Encode.float <| toRgbaList color )
        ]


{-| Render a textbox with multiple fonts.
-}
textboxMF : ( Float, Float ) -> Float -> String -> List String -> Color -> Renderable
textboxMF ( x, y ) size text fonts color =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textbox" )
        , ( "text", Encode.string text )
        , ( "size", Encode.float size )
        , ( "offset", Encode.list Encode.float [ x, y ] )
        , ( "fonts", Encode.list Encode.string fonts )
        , ( "color", Encode.list Encode.float <| toRgbaList color )
        ]


{-| Full TextBox options.
-}
type alias TextBoxOption =
    { fonts : List String
    , text : String
    , size : Float
    , color : Color
    , wordBreak : Bool
    , thickness : Maybe Float
    , italic : Maybe Float
    , width : Maybe Float
    , lineHeight : Maybe Float
    , wordSpacing : Maybe Float
    , align : Maybe String
    , tabSize : Maybe Float
    , baseline : Maybe String
    , letterSpacing : Maybe Float
    }


{-| Default TextBox options.
-}
defaultTextBoxOption : TextBoxOption
defaultTextBoxOption =
    { fonts = [ "consolas" ]
    , text = ""
    , size = 24
    , color = Color.black
    , wordBreak = False
    , thickness = Nothing
    , italic = Nothing
    , width = Nothing
    , lineHeight = Nothing
    , wordSpacing = Nothing
    , align = Nothing
    , tabSize = Nothing
    , baseline = Nothing
    , letterSpacing = Nothing
    }


{-| Render a textbox with more options.
-}
textboxPro : ( Float, Float ) -> TextBoxOption -> Renderable
textboxPro ( x, y ) opt =
    genProg
        [ ( "_c", Encode.int 0 )
        , ( "_p", Encode.string "textbox" )
        , ( "text", Encode.string opt.text )
        , ( "size", Encode.float opt.size )
        , ( "offset", Encode.list Encode.float [ x, y ] )
        , ( "fonts", Encode.list Encode.string opt.fonts )
        , ( "color", Encode.list Encode.float <| toRgbaList opt.color )
        , ( "wordBreak", Encode.bool opt.wordBreak )
        , ( "align", Encode.string <| Maybe.withDefault "left" opt.align )
        , ( "baseline", Encode.string <| Maybe.withDefault "top" opt.baseline )
        , ( "width", Encode.float <| Maybe.withDefault -1 opt.width )
        , ( "lineHeight", Encode.float <| Maybe.withDefault 1 opt.lineHeight )
        , ( "wordSpacing", Encode.float <| Maybe.withDefault 1 opt.wordSpacing )
        , ( "letterSpacing", Encode.float <| Maybe.withDefault 0 opt.letterSpacing )
        , ( "tabSize", Encode.float <| Maybe.withDefault 4 opt.tabSize )
        , ( "thickness", Encode.float <| Maybe.withDefault 0.5 opt.thickness )
        , ( "it", Encode.float <| Maybe.withDefault 0 opt.italic )
        ]


{-| Save the current FBO as a texture.

This is only an experimental feature and should be used with caution.

-}
saveAsTexture : String -> Renderable
saveAsTexture text =
    genProg
        [ ( "_c", Encode.int 4 )
        , ( "_n", Encode.string text )
        ]
