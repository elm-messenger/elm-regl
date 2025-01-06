module REGL.Compositors exposing
    ( dstOverSrc, maskBySrc
    , linearFade, imgFade
    )

{-|


# Compositors

@docs dstOverSrc, maskBySrc
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


{-| Fading effect using mask image.
-}
imgFade : String -> Float -> Renderable -> Renderable -> Renderable
imgFade mask t src dst =
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
                    ]
              )
            ]


{-| Linear fading effect using computed shader.
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
