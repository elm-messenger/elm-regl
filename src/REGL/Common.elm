module REGL.Common exposing (Effect, Renderable, genProg, group, render)

import Json.Encode as Encode exposing (Value)


type alias Effect =
    Value


type Renderable
    = AtomicRenderable Value
    | GroupRenderable (List Effect) (List Renderable)


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


group : List Effect -> List Renderable -> Renderable
group effects renderables =
    GroupRenderable effects renderables


genProg : Value -> Renderable
genProg =
    AtomicRenderable
