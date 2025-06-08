port module Mask exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, createREGLProgram, loadTexture, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P
import REGL.Common exposing (Effect, Renderable, group, render)
import REGL.Compositors
import REGL.Effects as E
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
    { starttime : Float
    , lasttime : Float
    , loadednum : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { starttime = 0
      , lasttime = 0
      , loadednum = 0
      }
    , Cmd.batch
        (batchExec execREGLCmd
            [ startREGL (REGLStartConfig 1920 1080 5 Nothing)
            , createREGLProgram "mask" prog
            , loadTexture "mask" "assets/img/mask.jpg" Nothing
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


genRenderable : Model -> Renderable
genRenderable model =
    let
        t =
            if model.lasttime > 10 then
                1

            else
                model.lasttime / 10
    in
    REGL.Compositors.imgFade "mask" t False (genRenderable1 model) (genRenderable2 model)


genRenderable1 : Model -> Renderable
genRenderable1 model =
    group
        (E.gblur 1)
        [ P.clear (Color.rgba 1 1 1 1)
        , P.triangle ( 0, 0 ) ( 1920, 0 ) ( 1920, 1080 ) (Color.rgba 1 0 0 1)
        , P.triangle ( 0, 0 ) ( 1920 / 2, 0 ) ( 1920 / 2, 1080 ) (Color.rgba 0 1 0 1)
        , P.triangle ( 1920 / 2, 0 ) ( 1920, 0 ) ( 1920, 1080 ) (Color.rgba 0 1 0 0.5)
        ]


genRenderable2 : Model -> Renderable
genRenderable2 model =
    group
        [ E.crt 50 ]
        [ P.clear (Color.rgba 1 1 1 1)
        , P.triangle ( 0, 0 ) ( 1920, 0 ) ( 960, 1080 ) (Color.rgba 0 0 1 1)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            if model.loadednum < 2 then
                ( model, setView Encode.null )

            else
                ( { model
                    | lasttime =
                        if model.starttime == 0 then
                            0

                        else
                            (t - model.starttime) / 1000
                    , starttime =
                        if model.starttime == 0 then
                            t

                        else
                            model.starttime
                  }
                , Cmd.batch
                    [ setView <| render <| genRenderable model
                    ]
                )

        REGLRecv x ->
            let
                cmd =
                    Decode.decodeValue (Decode.at [ "_c" ] Decode.string) x
            in
            case cmd of
                Ok "loadTexture" ->
                    ( { model | loadednum = model.loadednum + 1 }, Cmd.none )

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
uniform sampler2D texture;
uniform sampler2D mask;
uniform float t;
uniform vec2 view;
varying vec2 vuv;
void main() {
    float t0 = texture2D(mask, vec2(vuv.x, 1.-vuv.y)).x;
    t0 = t0 * .5 + .5;
    float a = smoothstep(-0.5, 0.,  (t - t0));
    gl_FragColor = mix(texture2D(texture, vuv), vec4(0., 0., 0., 1.), a);
}
"""


prog : REGLProgram
prog =
    REGL.Program.makeEffectSimple frag [ ( "t", DynamicValue "t" ), ( "mask", DynamicTextureValue "mask" ) ]


mask : Float -> Effect
mask t =
    [ ( "_p", Encode.string "mask" )
    , ( "t", Encode.float t )
    , ( "mask", Encode.string "mask" )
    ]
