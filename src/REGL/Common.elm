module REGL.Common exposing (..)

import Json.Encode exposing (Value)


type Renderable
    = AtomicRenderable Value
    | GroupRenderable (List Renderable)


type Color
    = ColorRGBA Float Float Float Float
