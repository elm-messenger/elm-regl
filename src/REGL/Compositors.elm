module REGL.Compositors exposing (dstOverSrc,maskBySrc)

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
