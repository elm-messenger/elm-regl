port module Particle exposing (..)

import Browser
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, createREGLProgram, genProg, primitiveToValue, render, startREGL, toHtmlWith, triangle)
import REGL.Common exposing (Renderable)
import REGL.Compositors
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
            [ startREGL (REGLStartConfig 1920 1080 5 Nothing)
            , createREGLProgram "particle" prog
            , createREGLProgram "myblur" blurprog
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


genRenderable : Model -> Renderable
genRenderable model =
    REGL.group []
        [ REGL.clear (Color.rgba 0 0 0 0)
        , particles model.lasttime

        -- , triangle (0, 0) (1920, 0) (1920, 1080) (Color.rgba 1 0 0 1)
        -- , triangle (0, 0) (1920/2, 0) (1920/2, 1080) (Color.rgba 0 1 0 1)
        -- , triangle ( 1920 / 2, 0 ) ( 1920 , 0 ) ( 1920, 1080 ) (Color.rgba 0 1 0 0.5)
        -- , REGL.Compositors.dstOverSrc
        --     (REGL.group
        --         [ REGL.clear (Color.rgba 0 0 0 0)
        --         -- , triangle ( 0, 0 ) ( 1920, 0 ) ( 1920, 1080 ) (Color.rgba 1 0 0 1)
        --         ]
        --     )
        --     (REGL.group
        --         [ REGL.clear (Color.rgba 0 0 0 0)
        --         , triangle ( 0, 0 ) ( 1920 / 2, 0 ) ( 1920 / 2, 1080 ) (Color.rgba 0 1 0 1)
        --         ]
        --     )
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
varying vec2 vRandom;
uniform float t;
void main() {
    vec2 uv = gl_PointCoord.xy;

    float d = length(uv - 0.5);
    float circle = 1. - smoothstep(0.4, 0.5, d);

    gl_FragColor.rgb = (0.6 + 0.3 * sin(uv.yxx + t + vRandom.x * 6.28) + vec3(0.1, 0.0, 0.3))*circle;
    gl_FragColor.a = circle;
}
"""


vert =
    """
precision mediump float;
attribute vec2 pos;
uniform vec2 view;
uniform float t;

varying vec2 vRandom;
void main() {
    vec2 mpos = pos;
    vRandom = vec2(sin(mpos.x * 12. + mpos.y * 78.), sin(-mpos.x * 44. + mpos.y * 23.));
    float mt = t * 0.6;
    mpos.x += sin(mt * vRandom.x + 6.28 * vRandom.y) * mix(0., 400., vRandom.x);
    mpos.y += cos(mt * vRandom.y + 6.28 * vRandom.x) * mix(0., 300., vRandom.y);
    gl_Position = vec4((mpos.x / view.x) * 2. - 1., (mpos.y / view.y) * 2. - 1., 0, 1);
    gl_PointSize = 10. + vRandom.x * 5.;
}
"""


prog : REGLProgram
prog =
    let
        len =
            2000

        randomPos =
            List.map (\x -> sin (toFloat x * 7.34) * 500 + 500) (List.range 1 (len * 2))
    in
    { frag = frag
    , vert = vert
    , attributes =
        Just
            [ ( "pos"
              , StaticValue (Encode.list Encode.float randomPos)
              )
            ]
    , uniforms =
        Just
            [ ( "t", DynamicValue "t" )
            ]
    , elements = Nothing
    , count = Just <| StaticValue (Encode.int len)
    , primitive = Just (StaticValue (primitiveToValue REGL.Points))
    }


particles : Float -> Renderable
particles t =
    genProg <|
        Encode.object
            [ ( "cmd", Encode.int 0 )
            , ( "prog", Encode.string "particle" )
            , ( "args"
              , Encode.object
                    [ ( "t", Encode.float t )
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
