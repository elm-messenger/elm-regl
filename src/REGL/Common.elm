module REGL.Common exposing
    ( Renderable(..), Effect, ProgramCall, Camera
    , genProg, group, groupWithCamera, render
    , getField, updateField
    , toRgbaList
    )

{-|


# Common Type Definitions

@docs Renderable, Effect, ProgramCall, Camera


## Common functions

@docs genProg, group, groupWithCamera, render


## Convenient helper functions

@docs getField, updateField
@docs toRgbaList

-}

import Color exposing (Color)
import Json.Encode as Encode exposing (Value)


{-| A call to the real REGL program.
-}
type alias ProgramCall =
    List ( String, Value )


{-| A post-processing effect.
-}
type alias Effect =
    ProgramCall


{-| Camera settings.
-}
type alias Camera =
    { x : Float
    , y : Float
    , zoom : Float
    , rotation : Float
    }


{-| A renderable object that can be rendered by REGL.
-}
type Renderable
    = AtomicRenderable ProgramCall
    | GroupRenderable (List Effect) (List Renderable)
    | GroupRenderableWithCamera Camera (List Effect) (List Renderable)


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
                , ( "_c", Encode.int 2 )
                ]

        GroupRenderableWithCamera camera effects renderables ->
            Encode.object
                [ ( "e", Encode.list (\e -> Encode.object e) effects )
                , ( "c", Encode.list identity (List.map render renderables) )
                , ( "_c", Encode.int 2 )
                , ( "_sc", Encode.list Encode.float [ camera.x, camera.y, camera.zoom, camera.rotation ] )
                ]


{-| Group a list of renderables into a single renderable with effects.
-}
group : List Effect -> List Renderable -> Renderable
group effects renderables =
    GroupRenderable effects renderables


{-| Group a list of renderables into a single renderable with effects and camera.

The camera will override current camera if exists.

-}
groupWithCamera : Camera -> List Effect -> List Renderable -> Renderable
groupWithCamera camera effects renderables =
    GroupRenderableWithCamera camera effects renderables


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


{-| Convert a color to a list of RGBA values.
-}
toRgbaList : Color -> List Float
toRgbaList c =
    let
        rgba =
            Color.toRgba c
    in
    [ rgba.red, rgba.green, rgba.blue, rgba.alpha ]
