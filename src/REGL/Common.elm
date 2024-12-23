module REGL.Common exposing (..)

import Json.Encode as Encode


type alias Setting =
    ( String, Encode.Value )


type Command
    = REGLCommand String (List Setting)
    | DrawCommand String (List Setting)


type Renderable
    = AtomicRenderable (List Setting) Command
    | GroupRenderable (List Setting) (List Renderable)
