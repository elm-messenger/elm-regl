module REGL.Color exposing
    ( Color(..)
    , black
    , blue
    , brown
    , charcoal
    , darkBlue
    , darkBrown
    , darkCharcoal
    , darkGray
    , darkGreen
    , darkGrey
    , darkOrange
    , darkPurple
    , darkRed
    , darkYellow
    , gray
    , green
    , grey
    , lightBlue
    , lightBrown
    , lightCharcoal
    , lightGray
    , lightGreen
    , lightGrey
    , lightOrange
    , lightPurple
    , lightRed
    , lightYellow
    , orange
    , purple
    , red
    , toRgbaList
    , white
    , yellow
    )

{-|


# Color

A color type and a set of predefined colors.

It uses `Float` values between 0 and 255 for each channel.

-}


type Color
    = ColorRGBA Float Float Float Float


toRgbaList : Color -> List Float
toRgbaList (ColorRGBA r g b a) =
    [ r, g, b, a ]


{-| -}
lightRed : Color
lightRed =
    ColorRGBA 239 41 41 255


{-| -}
red : Color
red =
    ColorRGBA 204 0 0 255


{-| -}
darkRed : Color
darkRed =
    ColorRGBA 164 0 0 255


{-| -}
lightOrange : Color
lightOrange =
    ColorRGBA 252 175 62 255


{-| -}
orange : Color
orange =
    ColorRGBA 245 121 0 255


{-| -}
darkOrange : Color
darkOrange =
    ColorRGBA 206 92 0 255


{-| -}
lightYellow : Color
lightYellow =
    ColorRGBA 255 233 79 255


{-| -}
yellow : Color
yellow =
    ColorRGBA 237 212 0 255


{-| -}
darkYellow : Color
darkYellow =
    ColorRGBA 196 160 0 255


{-| -}
lightGreen : Color
lightGreen =
    ColorRGBA 138 226 52 255


{-| -}
green : Color
green =
    ColorRGBA 115 210 22 255


{-| -}
darkGreen : Color
darkGreen =
    ColorRGBA 78 154 6 255


{-| -}
lightBlue : Color
lightBlue =
    ColorRGBA 114 159 207 255


{-| -}
blue : Color
blue =
    ColorRGBA 52 101 164 255


{-| -}
darkBlue : Color
darkBlue =
    ColorRGBA 32 74 135 255


{-| -}
lightPurple : Color
lightPurple =
    ColorRGBA 173 127 168 255


{-| -}
purple : Color
purple =
    ColorRGBA 117 80 123 255


{-| -}
darkPurple : Color
darkPurple =
    ColorRGBA 92 53 102 255


{-| -}
lightBrown : Color
lightBrown =
    ColorRGBA 233 185 110 255


{-| -}
brown : Color
brown =
    ColorRGBA 193 125 17 255


{-| -}
darkBrown : Color
darkBrown =
    ColorRGBA 143 89 2 255


{-| -}
black : Color
black =
    ColorRGBA 0 0 0 255


{-| -}
white : Color
white =
    ColorRGBA 255 255 255 255


{-| -}
lightGrey : Color
lightGrey =
    ColorRGBA 238 238 236 255


{-| -}
grey : Color
grey =
    ColorRGBA 211 215 207 255


{-| -}
darkGrey : Color
darkGrey =
    ColorRGBA 186 189 182 255


{-| -}
lightGray : Color
lightGray =
    ColorRGBA 238 238 236 255


{-| -}
gray : Color
gray =
    ColorRGBA 211 215 207 255


{-| -}
darkGray : Color
darkGray =
    ColorRGBA 186 189 182 255


{-| -}
lightCharcoal : Color
lightCharcoal =
    ColorRGBA 136 138 133 255


{-| -}
charcoal : Color
charcoal =
    ColorRGBA 85 87 83 255


{-| -}
darkCharcoal : Color
darkCharcoal =
    ColorRGBA 46 52 54 255
