module REGL.Program exposing
    ( ProgValue(..)
    , REGLProgram, Primitive(..), primitiveToValue, encodeProgram
    , makeCompositorProgram, makeEffectProgram, makeEffectSimple, makeCompositorSimple
    )

{-|


# Custom Program


## Program Value

@docs ProgValue


## Program Construction

@docs REGLProgram, Primitive, primitiveToValue, encodeProgram

@docs makeCompositorProgram, makeEffectProgram, makeEffectSimple, makeCompositorSimple

-}

import Json.Encode as Encode exposing (Value)


{-| A value that can be either a static value or a dynamic value
-}
type ProgValue
    = DynamicValue String
    | StaticValue Value
    | DynamicTextureValue String -- Dynamic texture value


{-| OpenGL primitive types
-}
type Primitive
    = Points
    | Lines
    | LineLoop
    | LineStrip
    | Triangles
    | TriangleStrip
    | TriangleFan


{-| A custom program that can be used with REGL
-}
type alias REGLProgram =
    { frag : String
    , vert : String
    , attributes : Maybe (List ( String, ProgValue ))
    , uniforms : Maybe (List ( String, ProgValue ))
    , elements : Maybe ProgValue
    , primitive : Maybe ProgValue
    , count : Maybe Int
    }


primitiveToValue : Primitive -> Value
primitiveToValue p =
    case p of
        Points ->
            Encode.string "points"

        Lines ->
            Encode.string "lines"

        LineLoop ->
            Encode.string "line loop"

        LineStrip ->
            Encode.string "line strip"

        Triangles ->
            Encode.string "triangles"

        TriangleStrip ->
            Encode.string "triangle strip"

        TriangleFan ->
            Encode.string "triangle fan"


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
    , Maybe.map (\x -> ( "count", Encode.int x )) p.count
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
-}
makeEffectProgram : String -> REGLProgram -> REGLProgram
makeEffectProgram texname p =
    let
        newuniform =
            case p.uniforms of
                Just x ->
                    ( texname, DynamicTextureValue "texture" ) :: x

                Nothing ->
                    [ ( texname, DynamicTextureValue "texture" ) ]
    in
    { p
        | uniforms = Just newuniform
    }


{-| Make a custom effect program simply.
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
    , uniforms = Just (( "texture", DynamicTextureValue "texture" ) :: uniforms)
    , elements = Just (StaticValue (Encode.list Encode.int [ 0, 1, 2, 0, 2, 3 ]))
    , primitive = Nothing
    , count = Nothing
    }


{-| Make a custom composite program.
-}
makeCompositorProgram : String -> String -> REGLProgram -> REGLProgram
makeCompositorProgram src dst p =
    let
        newuniform =
            case p.uniforms of
                Just x ->
                    ( src, DynamicTextureValue "t1" ) :: ( dst, DynamicTextureValue "t2" ) :: x

                Nothing ->
                    [ ( src, DynamicTextureValue "t1" ), ( dst, DynamicTextureValue "t2" ) ]
    in
    { p
        | uniforms = Just newuniform
    }


{-| Make a custom composite program simply.
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
    , uniforms = Just (( "t1", DynamicTextureValue "t1" ) :: ( "t2", DynamicTextureValue "t2" ) :: uniforms)
    , elements = Just (StaticValue (Encode.list Encode.int [ 0, 1, 2, 0, 2, 3 ]))
    , primitive = Nothing
    , count = Nothing
    }
