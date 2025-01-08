module REGL exposing
    ( Renderable, genProg, group, empty, render, Effect
    , clear, triangle, quad, texture, rectTexture
    , textbox, circle, centeredTexture, polyPrim, poly
    , lines, linestrip, lineloop, functionCurve
    , Primitive(..), primitiveToValue
    , REGLConfig, TimeInterval(..), configREGL
    , REGLStartConfig
    , TextureMagOption(..), TextureMinOption(..), TextureOptions, batchExec, createREGLProgram, loadTexture, startREGL, loadMSDFFont
    , decodeRecvMsg, Texture, REGLRecvMsg(..)
    , blur, gblur, crt, fxaa, alphamult
    , toHtmlWith, toRgbaList
    , saveAsTexture
    )

{-|


# REGL

This module exposes basic primitives for rendering with REGL.


## Basics

@docs Renderable, genProg, group, empty, render, Effect


## Builtin Commands

@docs clear, triangle, quad, texture, rectTexture
@docs textbox, circle, centeredTexture, polyPrim, poly
@docs lines, linestrip, lineloop, functionCurve

@docs Primitive, primitiveToValue


## User Configuration

@docs REGLConfig, TimeInterval, configREGL


## Direct REGL Commands

@docs REGLStartConfig

@docs TextureMagOption, TextureMinOption, TextureOptions, batchExec, createREGLProgram, loadTexture, startREGL, loadMSDFFont

@docs decodeRecvMsg, Texture, REGLRecvMsg


## Effects

@docs blur, gblur, crt, fxaa, alphamult


## Miscellaneous

@docs toHtmlWith, toRgbaList


## Advanced

@docs saveAsTexture

-}

import Color exposing (Color)
import Html exposing (Attribute, Html, canvas)
import Html.Attributes exposing (height, id, width)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import REGL.Common as C
import REGL.Program exposing (REGLProgram, encodeProgram)


{-| A renderable object that can be rendered by REGL.
-}
type alias Renderable =
    C.Renderable


{-| A post-processing effect.
-}
type alias Effect =
    C.Effect


{-| Generate a renderable object from settings. Users can use this function to create custom renderable generators.
-}
genProg : Value -> Renderable
genProg =
    C.genProg


{-| Group a list of renderables into a single renderable with effects.
-}
group : List Effect -> List Renderable -> Renderable
group =
    C.group


{-| An empty renderable object.
-}
empty : Renderable
empty =
    genProg Encode.null


{-| Render a renderable object. Users need to use it to pass the render result to REGL in JS through a port.
-}
render : Renderable -> Value
render =
    C.render


{-| Convert a color to a list of RGBA values.
-}
toRgbaList : Color -> List Float
toRgbaList c =
    let
        rgba =
            Color.toRgba c
    in
    [ rgba.red, rgba.green, rgba.blue, rgba.alpha ]


{-| A time interval for the REGL configuration.

Use `Millisecond` for fixed time intervals, and `AnimationFrame` for rendering at the refresh rate of the display.

`Millisecond` should be considered as a debugging tool, and `AnimationFrame` should be used for production.

-}
type TimeInterval
    = AnimationFrame
    | Millisecond Float


{-| The user configuration for REGL. Differing from the starting options. Users could change this at runtime.
-}
type alias REGLConfig =
    { timeInterval : TimeInterval
    }


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
textbox : ( Float, Float ) -> Float -> String -> String -> Renderable
textbox ( x, y ) size text font =
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


type alias ExecPort msg =
    Value -> Cmd msg



-- Direct commands


{-| The magnification option for textures.

Users should use `MagLinear` for most cases.
However, if pixel art is used, `MagNearest` should be used.

-}
type TextureMagOption
    = MagNearest
    | MagLinear


{-| The minification option for textures.

Similar to the magnification option, users should use `MinLinear` for most cases.

-}
type TextureMinOption
    = MinNearest
    | MinLinear
    | NearestMipmapNearest
    | LinearMipmapNearest
    | NearestMipmapLinear
    | LinearMipmapLinear


{-| The texture options.
-}
type alias TextureOptions =
    { mag : Maybe TextureMagOption
    , min : Maybe TextureMinOption
    }


{-| Execute a batch of commands.
-}
batchExec : ExecPort msg -> List (ExecPort msg -> Cmd msg) -> List (Cmd msg)
batchExec execPort cmds =
    List.map (\cmd -> cmd execPort) cmds


encodeTextureOptions : Maybe TextureOptions -> List ( String, Value )
encodeTextureOptions topts =
    case topts of
        Just opts ->
            [ ( "mag"
              , Encode.string <|
                    case opts.mag of
                        Just MagNearest ->
                            "nearest"

                        Just MagLinear ->
                            "linear"

                        Nothing ->
                            "linear"
              )
            , ( "min"
              , Encode.string <|
                    case opts.min of
                        Just MinNearest ->
                            "nearest"

                        Just MinLinear ->
                            "linear"

                        Just NearestMipmapNearest ->
                            "nearest mipmap nearest"

                        Just LinearMipmapNearest ->
                            "linear mipmap nearest"

                        Just NearestMipmapLinear ->
                            "nearest mipmap linear"

                        Just LinearMipmapLinear ->
                            "linear mipmap linear"

                        Nothing ->
                            "linear"
              )
            ]

        Nothing ->
            [ ( "mag", Encode.string "linear" ), ( "min", Encode.string "linear" ) ]


{-| Load a texture with name, url and options.
-}
loadTexture : String -> String -> Maybe TextureOptions -> ExecPort msg -> Cmd msg
loadTexture name url topts execPort =
    execPort <|
        Encode.object
            [ ( "cmd", Encode.string "loadTexture" )
            , ( "name", Encode.string name )
            , ( "opts"
              , Encode.object
                    (( "data", Encode.string url ) :: encodeTextureOptions topts)
              )
            ]


{-| The configuration for starting REGL.

`virtWidth` and `virtHeight` are the virtual width and height users defined.

`fboNum` is the number of framebuffers created.

`builtinPrograms` is a list of built-in programs that users want to enable.

-}
type alias REGLStartConfig =
    { virtWidth : Float
    , virtHeight : Float
    , fboNum : Int
    , builtinPrograms : Maybe (List String)
    }


{-| Execute the start command for REGL.
-}
startREGL : REGLStartConfig -> ExecPort msg -> Cmd msg
startREGL config execPort =
    let
        olddef =
            [ ( "cmd", Encode.string "start" )
            , ( "virtWidth", Encode.float config.virtWidth )
            , ( "virtHeight", Encode.float config.virtHeight )
            , ( "fboNum", Encode.int config.fboNum )
            ]

        def =
            case config.builtinPrograms of
                Just progs ->
                    ( "programs", Encode.list Encode.string progs ) :: olddef

                Nothing ->
                    olddef
    in
    execPort <|
        Encode.object
            def


{-| Create a user-defined REGL program.
-}
createREGLProgram : String -> REGLProgram -> ExecPort msg -> Cmd msg
createREGLProgram name program execPort =
    execPort <|
        Encode.object
            [ ( "cmd", Encode.string "createGLProgram" )
            , ( "name", Encode.string name )
            , ( "proto", encodeProgram program )
            ]


{-| Reconfigure REGL at runtime.
-}
configREGL : REGLConfig -> ExecPort msg -> Cmd msg
configREGL config execPort =
    execPort <|
        Encode.object
            [ ( "cmd", Encode.string "config" )
            , ( "config", encodeConfig config )
            ]


{-| Load a MSDF font.
-}
loadMSDFFont : String -> String -> String -> ExecPort msg -> Cmd msg
loadMSDFFont name imgurl jsonurl execPort =
    execPort <|
        Encode.object
            [ ( "cmd", Encode.string "loadFont" )
            , ( "name", Encode.string name )
            , ( "img", Encode.string imgurl )
            , ( "json", Encode.string jsonurl )
            ]


{-| Blurs a renderable.
-}
blur : Float -> Effect
blur radius =
    Encode.object
        [ ( "prog", Encode.string "blur" )
        , ( "args"
          , Encode.object
                [ ( "radius", Encode.float radius )
                ]
          )
        ]


{-| Multiply the alpha of a renderable.
-}
alphamult : Float -> Effect
alphamult a =
    Encode.object
        [ ( "prog", Encode.string "alphamult" )
        , ( "args"
          , Encode.object
                [ ( "alpha", Encode.float a )
                ]
          )
        ]


{-| Apply FXAA to a renderable.
-}
fxaa : Effect
fxaa =
    Encode.object
        [ ( "prog", Encode.string "fxaa" )
        ]


{-| Blurs a renderable. Gaussian blur.
-}
gblur : Float -> Effect
gblur sigma =
    Encode.object
        [ ( "prog", Encode.string "gblur" )
        , ( "args"
          , Encode.object
                [ ( "sigma", Encode.float sigma )
                ]
          )
        ]


{-| Simple CRT effect. Must be applied at the outermost layer.
-}
crt : Float -> Effect
crt count =
    Encode.object
        [ ( "prog", Encode.string "crt" )
        , ( "args"
          , Encode.object
                [ ( "count", Encode.float count )
                ]
          )
        ]


{-| Texture Information
-}
type alias Texture =
    { name : String
    , width : Int
    , height : Int
    }


{-| Receive messages from REGL.
-}
type REGLRecvMsg
    = REGLTextureLoaded Texture
    | REGLFontLoaded String
    | REGLProgramCreated String


{-| Decode a received message.
-}
decodeRecvMsg : Value -> Maybe REGLRecvMsg
decodeRecvMsg v =
    let
        cmd =
            Decode.decodeValue (Decode.at [ "cmd" ] Decode.string) v
    in
    case cmd of
        Ok "loadTexture" ->
            let
                w =
                    Result.withDefault 0 <| Decode.decodeValue (Decode.at [ "response", "width" ] Decode.int) v

                h =
                    Result.withDefault 0 <| Decode.decodeValue (Decode.at [ "response", "height" ] Decode.int) v

                txtname =
                    Result.withDefault "" <| Decode.decodeValue (Decode.at [ "response", "texture" ] Decode.string) v
            in
            Just (REGLTextureLoaded { name = txtname, width = w, height = h })

        Ok "loadFont" ->
            let
                name =
                    Result.withDefault "" <| Decode.decodeValue (Decode.at [ "response", "font" ] Decode.string) v
            in
            Just (REGLFontLoaded name)

        Ok "createGLProgram" ->
            let
                name =
                    Result.withDefault "" <| Decode.decodeValue (Decode.at [ "response", "name" ] Decode.string) v
            in
            Just (REGLProgramCreated name)

        _ ->
            Nothing
