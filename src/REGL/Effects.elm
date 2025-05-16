module REGL.Effects exposing (blur, gblur, crt, fxaa, alphamult)

{-|


# Effects

@docs blur, gblur, crt, fxaa, alphamult

-}

import Json.Encode as Encode
import REGL.Common exposing (Effect)


{-| Blurs a renderable.
-}
blur : Float -> Effect
blur radius =
    [ ( "prog", Encode.string "blur" )
    , ( "args"
      , Encode.object
            [ ( "radius", Encode.float radius )
            ]
      )
    ]


{-| Multiply the alpha of a renderable.
-}
alphamult : Float -> Effect
alphamult a =
    [ ( "prog", Encode.string "alphamult" )
    , ( "args"
      , Encode.object
            [ ( "alpha", Encode.float a )
            ]
      )
    ]


{-| Apply FXAA to a renderable.
-}
fxaa : Effect
fxaa =
    [ ( "prog", Encode.string "fxaa" )
    ]


{-| Blurs a renderable. Gaussian blur.
-}
gblur : Float -> Effect
gblur sigma =
    [ ( "prog", Encode.string "gblur" )
    , ( "args"
      , Encode.object
            [ ( "sigma", Encode.float sigma )
            ]
      )
    ]


{-| Simple CRT effect. Must be applied at the outermost layer.
-}
crt : Float -> Effect
crt count =
    [ ( "prog", Encode.string "crt" )
    , ( "args"
      , Encode.object
            [ ( "count", Encode.float count )
            ]
      )
    ]
