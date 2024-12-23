module REGL.Program exposing (..)

import Json.Encode as Encode exposing (Value)


type alias REGLProgram =
    { frag : String
    , vert : String
    , attributes : Maybe (List ( String, Value ))
    , uniforms : Maybe (List ( String, Value ))
    , textureUniformKeys : Maybe (List String)
    , elements : Maybe (List Int)
    , count : Int
    }


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Just a ->
            [ a ]

        Nothing ->
            []


encodeProgram : REGLProgram -> Value
encodeProgram p =
    Encode.object <|
        [ ( "frag", Encode.string p.frag )
        , ( "vert", Encode.string p.vert )
        , ( "count", Encode.int p.count )
        ]
            ++ (maybeToList <|
                    Maybe.map (\x -> ( "elements", Encode.list Encode.int x )) p.elements
               )
            ++ (maybeToList <|
                    Maybe.map (\x -> ( "attributes", Encode.object x )) p.attributes
               )
            ++ (maybeToList <|
                    Maybe.map (\x -> ( "uniforms", Encode.object x )) p.uniforms
               )
            ++ (maybeToList <|
                    Maybe.map (\x -> ( "textureUniformKeys", Encode.list Encode.string x )) p.textureUniformKeys
               )
