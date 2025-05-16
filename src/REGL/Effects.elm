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
    [ ( "_p", Encode.string "blur" )
    , ( "radius", Encode.float radius )
    ]


{-| Multiply the alpha of a renderable.
-}
alphamult : Float -> Effect
alphamult a =
    [ ( "_p", Encode.string "alphamult" )
    , ( "alpha", Encode.float a )
    ]


{-| Apply FXAA to a renderable.
-}
fxaa : Effect
fxaa =
    [ ( "_p", Encode.string "fxaa" )
    ]


{-| Blurs a renderable. Gaussian blur.
-}
gblur : Float -> Effect
gblur sigma =
    [ ( "_p", Encode.string "gblur" )
    , ( "sigma", Encode.float sigma )
    ]


{-| Simple CRT effect. Must be applied at the outermost layer.
-}
crt : Float -> Effect
crt count =
    [ ( "_p", Encode.string "crt" )
    , ( "count", Encode.float count )
    ]
