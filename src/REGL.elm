module REGL exposing
    ( empty
    , REGLConfig, TimeInterval(..), configREGL
    , REGLStartConfig
    , TextureMagOption(..), TextureMinOption(..), TextureOptions, batchExec, createREGLProgram, loadTexture, startREGL, loadMSDFFont
    , decodeRecvMsg, Texture, REGLRecvMsg(..)
    , toHtmlWith
    )

{-|


# REGL

This module exposes basic primitives for rendering with REGL.


## Basics

@docs Renderable, genProg, group, empty, render, Effect


## User Configuration

@docs REGLConfig, TimeInterval, configREGL


## Direct REGL Commands

@docs REGLStartConfig

@docs TextureMagOption, TextureMinOption, TextureOptions, batchExec, createREGLProgram, loadTexture, startREGL, loadMSDFFont

@docs decodeRecvMsg, Texture, REGLRecvMsg


## Miscellaneous

@docs toHtmlWith

-}

import Html exposing (Attribute, Html, canvas)
import Html.Attributes exposing (height, id, width)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import REGL.Common exposing (Renderable, genProg)
import REGL.Program exposing (REGLProgram, encodeProgram)


{-| An empty renderable object.
-}
empty : Renderable
empty =
    genProg []


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
    , crop : Maybe ( ( Int, Int ), ( Int, Int ) )
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
            , ( "subimg"
              , case opts.crop of
                    Just ( ( x, y ), ( w, h ) ) ->
                        Encode.list Encode.int [ x, y, w, h ]

                    Nothing ->
                        Encode.null
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
