module REGL.API exposing (..)

import Json.Encode as Encode exposing (Value)
import REGL.Common exposing (Color(..))


programBase : String -> List ( String, Value )
programBase name =
    [ ( "cmd", Encode.int 0 )
    , ( "program", Encode.string name )
    ]


rectProgram : List ( String, Value )
rectProgram =
    programBase "rect"


simpleTextureProgram : List ( String, Value )
simpleTextureProgram =
    programBase "simpTexture"


appendArgs : List ( String, Value ) -> List ( String, Value ) -> Value
appendArgs args program =
    Encode.object
        (( "args"
         , Encode.object args
         )
            :: program
        )


toRgbaList : Color -> List Float
toRgbaList (ColorRGBA r g b a) =
    [ r, g, b, a ]


clearREGL : Color -> Float -> List ( String, Encode.Value )
clearREGL color depth =
    [ ( "cmd", Encode.int 1 )
    , ( "name", Encode.string "clear" )
    , ( "args"
      , Encode.object
            [ ( "color", Encode.list Encode.float (toRgbaList color) )
            , ( "depth", Encode.float depth )
            ]
      )
    ]
