module REGL.Program exposing
    ( ProgValue(..)
    , REGLProgram, Primitive(..), primitiveToValue, encodeProgram
    )

{-|


# Custom Program


## Program Value

@docs ProgValue


## Program Construction

@docs REGLProgram, Primitive, primitiveToValue, encodeProgram

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


{-| Encode a custom program to an object
-}
encodeProgram : REGLProgram -> Value
encodeProgram p =
    Encode.object <|
        List.filterMap identity (encodeProgramHelper p)
