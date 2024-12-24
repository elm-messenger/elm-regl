module REGL.Triangle exposing (..)

import Json.Encode as Encode exposing (Value)
import REGL exposing (genProg)
import REGL.API exposing (appendArgs, programBase, toRgbaList)
import REGL.Common exposing (Color, Renderable)
import REGL.Program exposing (ProgValue(..), REGLProgram)


frag =
    """
precision mediump float;
uniform vec4 color;
void main() {
    gl_FragColor = color;
}
"""


vert =
    """
precision mediump float;
attribute vec2 pos;
void main() {
    gl_Position = vec4(pos, 0, 1);
}
"""


prog : REGLProgram
prog =
    { frag = frag
    , vert = vert
    , attributes =
        Just
            [ ( "pos"
              , DynamicValue "pos"
              )
            ]
    , uniforms =
        Just
            [ ( "color", DynamicValue "color" )
            ]
    , elements = Nothing
    , count = 3
    , primitive = Nothing
    }


triangleProgramHelper : List ( String, Value )
triangleProgramHelper =
    programBase "triangle"


triangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
triangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg <|
        appendArgs
            [ ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3 ] )
            , ( "color", Encode.list Encode.float (toRgbaList color) )
            ]
            triangleProgramHelper
