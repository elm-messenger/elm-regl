module REGL.Compositors exposing
    ( dstOverSrc, maskBySrc
    , linearFade, imgFade
    )

{-|


# Compositors

Compose two renderables using various shaders.


## Default Compositors

@docs dstOverSrc, maskBySrc


## Fading Compositor

@docs linearFade, imgFade

-}

import Json.Encode as Encode
import REGL.Common exposing (Renderable, genProg, render)


{-| Draw the dst renderable over the src renderable.
-}
dstOverSrc : Renderable -> Renderable -> Renderable
dstOverSrc src dst =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 3 )
            , ( "prog", Encode.string "defaultCompositor" )
            , ( "r1", render src )
            , ( "r2", render dst )
            , ( "args"
              , Encode.object
                    [ ( "mode", Encode.int 0 )
                    ]
              )
            ]


{-| Draw the dst renderable masked by the src renderable.
-}
maskBySrc : Renderable -> Renderable -> Renderable
maskBySrc src dst =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 3 )
            , ( "prog", Encode.string "defaultCompositor" )
            , ( "r1", render src )
            , ( "r2", render dst )
            , ( "args"
              , Encode.object
                    [ ( "mode", Encode.int 1 )
                    ]
              )
            ]


{-| Fading effect using a mask image.

Mask image is a gradient image, similar to [this](https://github.com/linsyking/elm-regl/blob/main/docs/asset/mask.jpg).

-}
imgFade : String -> Float -> Bool -> Renderable -> Renderable -> Renderable
imgFade mask t invert src dst =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 3 )
            , ( "prog", Encode.string "imgFade" )
            , ( "r1", render src )
            , ( "r2", render dst )
            , ( "args"
              , Encode.object
                    [ ( "mask", Encode.string mask )
                    , ( "t", Encode.float t )
                    , ( "invert_mask", Encode.int (if invert then 1 else 0) )
                    ]
              )
            ]


{-| Linear fading effect.
-}
linearFade : Float -> Renderable -> Renderable -> Renderable
linearFade t src dst =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 3 )
            , ( "prog", Encode.string "compFade" )
            , ( "r1", render src )
            , ( "r2", render dst )
            , ( "args"
              , Encode.object
                    [ ( "mode", Encode.int 0 )
                    , ( "t", Encode.float t )
                    ]
              )
            ]
