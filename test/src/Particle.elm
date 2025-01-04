port module Particle exposing (..)

import Browser
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, createREGLProgram, genProg, render, startREGL, toHtmlWith)
import REGL.Common exposing (Renderable)
import REGL.Program exposing (ProgValue(..), REGLProgram)


port setView : Encode.Value -> Cmd msg


port execREGLCmd : Encode.Value -> Cmd msg


port recvREGLCmd : (Encode.Value -> msg) -> Sub msg


port reglupdate : (Float -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { lasttime : Float
    , loadednum : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lasttime = 0
      , loadednum = 0
      }
    , Cmd.batch
        (batchExec execREGLCmd
            [ startREGL (REGLStartConfig 1920 1080)
            , createREGLProgram "mytriangle" prog
            , createREGLProgram "myblur" blurprog
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


genRenderable : Model -> Renderable
genRenderable model =
    REGL.groupEffects [ myblur 10 ]
        [ REGL.clear (Color.rgba 0 0 0 1)
        , mytriangle ( 0, 0 ) ( 1, 0 ) ( 0, 1 ) (Color.rgba 1 0 0 1)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            if model.loadednum < 1 then
                ( model, setView Encode.null )

            else
                ( { model | lasttime = t }
                , Cmd.batch
                    [ setView <| render <| genRenderable model
                    ]
                )

        REGLRecv x ->
            let
                cmd =
                    Decode.decodeValue (Decode.at [ "cmd" ] Decode.string) x
            in
            case cmd of
                Ok "createGLProgram" ->
                    ( { model | loadednum = model.loadednum + 1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ reglupdate Tick
        , recvREGLCmd REGLRecv
        ]


view : Model -> Html Msg
view _ =
    toHtmlWith { width = 1280, height = 720 }
        [ style "left" "0px"
        , style "top" "0px"
        , style "position" "fixed"
        ]


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
    , count = Just 3
    , primitive = Nothing
    }


mytriangle : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Color -> Renderable
mytriangle ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) color =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "mytriangle" )
            , ( "args"
              , Encode.object
                    [ ( "pos", Encode.list Encode.float [ x1, y1, x2, y2, x3, y3 ] )
                    , ( "color", Encode.list Encode.float (REGL.toRgbaList color) )
                    ]
              )
            ]


blurfrag =
    """
precision mediump float;
uniform sampler2D texture;
uniform float radius;
uniform vec2 view;
varying vec2 vuv;
void main() {
    if(radius < 0.1) {
        gl_FragColor = texture2D(texture, vuv);
        return;
    }
    vec3 avg = vec3(0.0);
    float maxa = 0.0;
    for(int x = -5; x <= 5; x++) {
        for(int y = -5; y <= 5; y++) {
            vec4 c = texture2D(texture, vuv + vec2(float(x) * radius / view.x, float(y) * radius / view.y));
            avg += (1.0 / 121.0) * c.xyz;
            maxa = max(maxa, c.a);
        }
    }
    gl_FragColor = vec4(avg, maxa);
}
"""


blurprog : REGLProgram
blurprog =
    REGL.Program.makeEffectSimple blurfrag [ ( "radius", DynamicValue "radius" ) ]


myblur : Float -> REGL.Effect
myblur radius =
    Encode.object
        [ ( "prog", Encode.string "myblur" )
        , ( "args"
          , Encode.object
                [ ( "radius", Encode.float radius )
                ]
          )
        ]
