module REGL.Common exposing
    ( Renderable(..), Effect, ProgramCall
    , genProg, group, render
    , getField, updateField
    )

{-|


# Common Type Definitions

@docs Renderable, Effect, ProgramCall


## Common functions

@docs genProg, group, render


## Convenient helper functions

@docs getField, updateField

-}

import Json.Encode as Encode exposing (Value)


{-| A call to the real REGL program.
-}
type alias ProgramCall =
    List ( String, Value )


{-| A post-processing effect.
-}
type alias Effect =
    ProgramCall


{-| A renderable object that can be rendered by REGL.
-}
type Renderable
    = AtomicRenderable ProgramCall
    | GroupRenderable (List Effect) (List Renderable)


{-| Render a renderable object. Users need to use it to pass the render result to REGL in JS through a port.
-}
render : Renderable -> Value
render renderable =
    case renderable of
        AtomicRenderable value ->
            Encode.object value

        GroupRenderable effects renderables ->
            Encode.object
                [ ( "e", Encode.list (\e -> Encode.object e) effects )
                , ( "c", Encode.list identity (List.map render renderables) )
                , ( "cmd", Encode.int 2 )
                ]


{-| Group a list of renderables into a single renderable with effects.
-}
group : List Effect -> List Renderable -> Renderable
group effects renderables =
    GroupRenderable effects renderables


updateListFoldr : String -> Value -> ProgramCall -> ProgramCall
updateListFoldr key newVal list =
    let
        ( updatedList, found ) =
            List.foldr
                (\( k, v ) ( acc, hasUpdated ) ->
                    if k == key then
                        ( ( k, newVal ) :: acc, True )

                    else
                        ( ( k, v ) :: acc, hasUpdated )
                )
                ( [], False )
                list
    in
    if found then
        updatedList

    else
        ( key, newVal ) :: updatedList


{-| A handy function to update a field of AtomicRenderable.
-}
updateField : String -> Value -> Renderable -> Renderable
updateField key value r =
    case r of
        AtomicRenderable ov ->
            AtomicRenderable <| updateListFoldr key value ov

        _ ->
            r


{-| A handy function to get a field of AtomicRenderable.
-}
getField : String -> Renderable -> Maybe Value
getField key r =
    case r of
        AtomicRenderable pc ->
            List.foldr
                (\( k, v ) acc ->
                    if k == key then
                        Just v

                    else
                        acc
                )
                Nothing
                pc

        _ ->
            Nothing


{-| Generate a renderable object from settings. Users can use this function to create custom renderable generators.
-}
genProg : ProgramCall -> Renderable
genProg =
    AtomicRenderable
