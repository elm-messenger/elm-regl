module REGL.Common exposing (..)

import Json.Encode exposing (Value)


type Renderable
    = AtomicRenderable Value
    | GroupRenderable (List Renderable)


type Color
    = ColorRGBA Float Float Float Float


type TimeInterval
    = AnimationFrame
    | Millisecond Float


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
    Json.Encode.object
        [ ( "interval", Json.Encode.float interval )
        ]
