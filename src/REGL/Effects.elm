module REGL.Effects exposing
    ( blur, blurh, blurv, gblur, gblurh, gblurv
    , alphamult, colormult
    , crt, pixilation
    , fxaa
    )

{-|


# Effects

@docs blur, blurh, blurv, gblur, gblurh, gblurv
@docs alphamult, colormult
@docs crt, pixilation
@docs fxaa

-}

import Json.Encode as Encode
import REGL.Common exposing (Effect)


{-| Blurs a renderable in 2 passes.
-}
blur : Float -> List Effect
blur radius =
    [ blurh radius, blurv radius ]


{-| Blurs a renderable horizontally.
-}
blurh : Float -> Effect
blurh radius =
    [ ( "_p", Encode.string "blurh" )
    , ( "radius", Encode.float radius )
    ]


{-| Blurs a renderable vertically.
-}
blurv : Float -> Effect
blurv radius =
    [ ( "_p", Encode.string "blurv" )
    , ( "radius", Encode.float radius )
    ]


{-| Multiply the alpha of a renderable.
-}
alphamult : Float -> Effect
alphamult a =
    [ ( "_p", Encode.string "alphamult" )
    , ( "alpha", Encode.float a )
    ]


{-| Multiply the RGBA of a renderable.
-}
colormult : Float -> Float -> Float -> Float -> Effect
colormult r g b a =
    [ ( "_p", Encode.string "colormult" )
    , ( "color", Encode.list Encode.float [ r, g, b, a ] )
    ]


{-| Apply pixilation effect to a renderable.
-}
pixilation : Float -> Effect
pixilation ps =
    [ ( "_p", Encode.string "pixilation" )
    , ( "ps", Encode.float ps )
    ]


{-| Apply FXAA to a renderable.
-}
fxaa : Effect
fxaa =
    [ ( "_p", Encode.string "fxaa" )
    ]


{-| Blurs a renderable using Gaussian blur.

Algorithm from <https://github.com/Experience-Monks/glsl-fast-gaussian-blur>.

This is a 9-tap Gaussian blur that applies 8 passes of Gaussian blur with decreasing radius, which is a good compromise between performance and quality.

This has similar effect with applying multiple `blur` effects with decreasing radius, but it is more efficient.

-}
gblur : Float -> List Effect
gblur radius =
    [ gblurh (radius * 8)
    , gblurv (radius * 7)
    , gblurh (radius * 6)
    , gblurv (radius * 5)
    , gblurh (radius * 4)
    , gblurv (radius * 3)
    , gblurh (radius * 2)
    , gblurv (radius * 1)
    ]


{-| A single horizontal pass that blurs a renderable using Gaussian blur.
-}
gblurh : Float -> Effect
gblurh r =
    [ ( "_p", Encode.string "gblurh" )
    , ( "radius", Encode.float r )
    ]


{-| A single vertical pass that blurs a renderable using Gaussian blur.
-}
gblurv : Float -> Effect
gblurv r =
    [ ( "_p", Encode.string "gblurv" )
    , ( "radius", Encode.float r )
    ]


{-| Simple CRT effect. Must be applied at the outermost layer.
-}
crt : Float -> Effect
crt count =
    [ ( "_p", Encode.string "crt" )
    , ( "count", Encode.float count )
    ]
