module REGL.Common exposing (Effect, Renderable, genProg, group, render)

{-|


# Common utilities for REGL

-}

import Json.Encode as Encode exposing (Value)


{-| A post-processing effect.
-}
type alias Effect =
    Value


{-| A renderable object that can be rendered by REGL.
-}
type Renderable
    = AtomicRenderable Value
    | GroupRenderable (List Effect) (List Renderable)


{-| Render a renderable object.
-}
render : Renderable -> Value
render renderable =
    case renderable of
        AtomicRenderable value ->
            value

        GroupRenderable effects renderables ->
            Encode.object
                [ ( "e", Encode.list identity effects )
                , ( "c", Encode.list identity (List.map render renderables) )
                , ( "cmd", Encode.int 2 )
                ]


-- renderHelper : Renderable -> List Value -> List Value
-- renderHelper renderable old =
--     case renderable of
--         AtomicRenderable value ->
--             old ++ [ value ]

--         GroupRenderable _ [] ->
--             old

--         GroupRenderable [] renderables ->
--             List.foldl renderHelper old renderables

--         GroupRenderable effects renderables ->
--             let
--                 rs =
--                     Encode.list identity (List.foldl renderHelper [] renderables)
--             in
--             old
--                 ++ [ Encode.object
--                         [ ( "e", Encode.list identity effects )
--                         , ( "c", rs )
--                         , ( "cmd", Encode.int 2 )
--                         ]
--                    ]


{-| Group a list of renderables into a single renderable.
-}
group : List Effect -> List Renderable -> Renderable
group effects renderables =
    GroupRenderable effects renderables


{-| Generate a renderable object from an object.
-}
genProg : Value -> Renderable
genProg =
    AtomicRenderable
