module REGL.Program exposing
    ( ProgValue(..)
    , REGLProgram, encodeProgram
    , makeEffectSimple, makeCompositorSimple
    , makeCompositorProgram, makeEffectProgram
    )

{-|


# Custom REGL Program

Users could define their own custom REGL program using this module.
REGL programs could be used as a renderable generator, effect, or compositor.


## Program Value

@docs ProgValue


## Program Construction

@docs REGLProgram, encodeProgram

Usually users could use the following functions to construct a custom program by only providing the fragment shader and uniforms:

@docs makeEffectSimple, makeCompositorSimple

If more customization is needed, users could use the following functions:

@docs makeCompositorProgram, makeEffectProgram

-}

import Json.Encode as Encode exposing (Value)


{-| A value that can be either a static value or a dynamic value.
-}
type ProgValue
    = DynamicValue String
    | StaticValue Value
    | DynamicTextureValue String -- Dynamic texture value


{-| A custom program that can be used with REGL.
-}
type alias REGLProgram =
    { frag : String
    , vert : String
    , attributes : Maybe (List ( String, ProgValue ))
    , uniforms : Maybe (List ( String, ProgValue ))
    , elements : Maybe ProgValue
    , primitive : Maybe ProgValue
    , count : Maybe ProgValue
    }


getDynamicValue : List ( String, ProgValue ) -> Value
getDynamicValue x =
    Encode.object <|
        List.filterMap
            (\( k, v ) ->
                case v of
                    DynamicValue s ->
                        Just ( k, Encode.string s )

                    _ ->
                        Nothing
            )
            x


getDynamicTextureValue : List ( String, ProgValue ) -> Value
getDynamicTextureValue x =
    Encode.object <|
        List.filterMap
            (\( k, v ) ->
                case v of
                    DynamicTextureValue s ->
                        Just ( k, Encode.string s )

                    _ ->
                        Nothing
            )
            x


getStaticValue : List ( String, ProgValue ) -> Value
getStaticValue x =
    Encode.object <|
        List.filterMap
            (\( k, v ) ->
                case v of
                    StaticValue s ->
                        Just ( k, s )

                    _ ->
                        Nothing
            )
            x


getStaticSingleProgValue : ProgValue -> Value
getStaticSingleProgValue v =
    case v of
        StaticValue s ->
            s

        _ ->
            Encode.null


getDynamicSingleProgValue : ProgValue -> Value
getDynamicSingleProgValue v =
    case v of
        DynamicValue s ->
            Encode.string s

        _ ->
            Encode.null


encodeProgramHelper : REGLProgram -> List (Maybe ( String, Value ))
encodeProgramHelper p =
    [ Just ( "frag", Encode.string p.frag )
    , Just ( "vert", Encode.string p.vert )
    , Maybe.map (\x -> ( "count", getStaticSingleProgValue x )) p.count
    , Maybe.map (\x -> ( "countDyn", getDynamicSingleProgValue x )) p.count
    , Maybe.map (\x -> ( "elements", getStaticSingleProgValue x )) p.elements
    , Maybe.map (\x -> ( "elementsDyn", getDynamicSingleProgValue x )) p.elements
    , Maybe.map (\x -> ( "primitive", getStaticSingleProgValue x )) p.primitive
    , Maybe.map (\x -> ( "primitiveDyn", getDynamicSingleProgValue x )) p.primitive
    , Maybe.map (\x -> ( "attributes", getStaticValue x )) p.attributes
    , Maybe.map (\x -> ( "attributesDyn", getDynamicValue x )) p.attributes
    , Maybe.map (\x -> ( "uniforms", getStaticValue x )) p.uniforms
    , Maybe.map (\x -> ( "uniformsDyn", getDynamicValue x )) p.uniforms
    , Maybe.map (\x -> ( "uniformsDynTexture", getDynamicTextureValue x )) p.uniforms
    ]


{-| Encode a custom program to an object.
-}
encodeProgram : REGLProgram -> Value
encodeProgram p =
    Encode.object <|
        List.filterMap identity (encodeProgramHelper p)


{-| Make a custom effect program.

`texname` is the name of the texture uniform in the program.

-}
makeEffectProgram : String -> REGLProgram -> REGLProgram
makeEffectProgram texname p =
    let
        newuniform =
            case p.uniforms of
                Just x ->
                    ( texname, DynamicValue "texture" ) :: x

                Nothing ->
                    [ ( texname, DynamicValue "texture" ) ]
    in
    { p
        | uniforms = Just newuniform
    }


{-| Make a custom effect program simply.

In the fragment shader, use `texture2D(texture, vuv)` to sample the texture.

You could also use `vec2 view` to get the size of virtual canvas.

-}
makeEffectSimple : String -> List ( String, ProgValue ) -> REGLProgram
makeEffectSimple frag uniforms =
    { frag = frag
    , vert = "precision mediump float; attribute vec2 uv; varying vec2 vuv; void main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}"
    , attributes =
        Just
            [ ( "uv"
              , StaticValue
                    (Encode.list Encode.float
                        [ 1
                        , 1
                        , 1
                        , 0
                        , 0
                        , 0
                        , 0
                        , 1
                        ]
                    )
              )
            ]
    , uniforms = Just (( "texture", DynamicValue "texture" ) :: uniforms)
    , elements = Just (StaticValue (Encode.list Encode.int [ 0, 1, 2, 0, 2, 3 ]))
    , primitive = Nothing
    , count = Nothing
    }


{-| Make a custom composite program.

`src` and `dst` are the names of the texture uniforms in the program.

-}
makeCompositorProgram : String -> String -> REGLProgram -> REGLProgram
makeCompositorProgram src dst p =
    let
        newuniform =
            case p.uniforms of
                Just x ->
                    ( src, DynamicValue "t1" ) :: ( dst, DynamicValue "t2" ) :: x

                Nothing ->
                    [ ( src, DynamicValue "t1" ), ( dst, DynamicValue "t2" ) ]
    in
    { p
        | uniforms = Just newuniform
    }


{-| Make a custom composite program simply.

`t1` and `t2` are the names of the texture uniforms in the program.

-}
makeCompositorSimple : String -> List ( String, ProgValue ) -> REGLProgram
makeCompositorSimple frag uniforms =
    { frag = frag
    , vert = "precision mediump float; attribute vec2 uv; varying vec2 vuv; void main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}"
    , attributes =
        Just
            [ ( "uv"
              , StaticValue
                    (Encode.list Encode.float
                        [ 1
                        , 1
                        , 1
                        , 0
                        , 0
                        , 0
                        , 0
                        , 1
                        ]
                    )
              )
            ]
    , uniforms = Just (( "t1", DynamicValue "t1" ) :: ( "t2", DynamicValue "t2" ) :: uniforms)
    , elements = Just (StaticValue (Encode.list Encode.int [ 0, 1, 2, 0, 2, 3 ]))
    , primitive = Nothing
    , count = Nothing
    }
