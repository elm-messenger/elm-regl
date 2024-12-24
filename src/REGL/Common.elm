module REGL.Common exposing (Renderable, genProg, group, render)

{-|


# Common utilities for REGL

-}

import Json.Encode as Encode exposing (Value)


{-| A renderable object that can be rendered by REGL.
-}
type Renderable
    = AtomicRenderable Value
    | GroupRenderable (List Renderable)


{-| Render a renderable object.
-}
render : Renderable -> Value
render renderable =
    case renderable of
        AtomicRenderable value ->
            value

        GroupRenderable renderables ->
            Encode.list identity (List.map render renderables)


{-| Group a list of renderables into a single renderable.
-}
group : List Renderable -> Renderable
group renderables =
    GroupRenderable renderables


{-| Generate a renderable object from an object.
-}
genProg : Value -> Renderable
genProg =
    AtomicRenderable
