port module Basic exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadTexture, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P
import REGL.Common exposing (Renderable, group, render)
import REGL.Effects as E
import String exposing (fromInt)


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
    , ts : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { starttime = 0
      , lasttime = 0
      , loadednum = 0
      , ts = ( 0, 0 )
      }
    , Cmd.batch
        (batchExec execREGLCmd
            [ loadTexture "enemy" "assets/enemy.png" Nothing
            , startREGL (REGLStartConfig 1920 1080 5 Nothing)
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


lorem =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus tortor massa, hendrerit eu tellus non, iaculis cursus purus. Phasellus at tempor ipsum. Quisque efficitur tortor sed tincidunt elementum. Aliquam erat volutpat. Morbi eu diam a mauris venenatis tincidunt eu et diam. In hac habitasse platea dictumst. Curabitur vitae massa at justo pellentesque molestie nec a diam. Fusce sed neque neque. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Etiam ut augue gravida, dictum felis in, semper nisi. Vestibulum a odio quis neque lobortis luctus eget at orci."""


genRenderable : Model -> Renderable
genRenderable model =
    let
        ( w, h ) =
            model.ts

        step =
            2
    in
    group []
        -- group (E.blur 3 ++ E.blur 2 ++ E.blur 1)
        [ P.clear (Color.rgba 1 1 1 1)
        , P.textbox ( 0, 0 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
        , P.quad ( 0, 0 ) ( 1920, 0 ) ( 1920 / 3, 1080 / 3 ) ( 0, 1080 ) (Color.rgba 1 0.2 0.4 1)
        , P.textboxPro ( 100, 100 ) (P.TextBoxOption "consolas" lorem 70 (Color.rgba 0.5 0.5 1.0 0.5) False (Just 1) (Just 4) (Just 1700) Nothing Nothing Nothing Nothing)
        , P.textbox ( 100, 500 ) 100 "[BIG]" "consolas" Color.black
        , P.textbox ( 100, 1000 ) 20 "[small]" "consolas" Color.black
        , P.textbox ( 100, 1020 ) 30 "[medium]" "consolas" Color.black
        , group []
            [ P.triangle ( 700, 100 ) ( 700 + 100, 100 ) ( 700 + 100, 100 / 2 ) Color.red
            , P.triangle ( 500, 100 ) ( 500 + 100, 100 ) ( 500 + 100, 100 / 2 ) Color.green
            ]
        , P.poly
            [ ( 1100, 600 )
            , ( 1100, 650 )
            , ( 1200, 680 )
            , ( 1300, 650 )
            , ( 1200, 600 )
            ]
            Color.blue
        , P.texture ( 0, 0 ) ( w, 0 ) ( w, h ) ( 0, h ) "enemy"
        , P.rectTextureCropped ( 100, 600 ) ( w / 2, h ) ( 0, 0 ) ( 0.5, 1 ) "enemy"
        , P.centeredTextureCropped ( 1100, 600 ) ( w / 2, h ) 45 ( 0, 0 ) ( 0.5, 1 ) "enemy"
        , P.circle ( 1100, 600 ) 10 Color.black
        , P.centeredTexture ( 1400, 300 ) ( w, h ) (model.lasttime / 5) "enemy"
        , P.circle ( 1400, 300 ) 30 Color.black
        , P.quad ( 500, 500 ) ( 800, 500 ) ( 800, 900 ) ( 500, 900 ) (Color.rgba 0 0 0 0.1)
        , group (E.blur 1)
            [ P.quad ( 1500, 500 ) ( 1800, 500 ) ( 1800, 900 ) ( 1500, 900 ) (Color.rgba 0.4 0.7 0.9 1)
            ]
        , P.textbox ( 1510, 510 ) 30 "Hello\nThis is a clear text\n on a blurred\nbackground." "consolas" Color.black
        , P.lineloop
            [ ( 100, 100 )
            , ( 200, 100 )
            , ( 300, 200 )
            , ( 100, 200 )
            ]
            Color.black
        , P.lines
            [ ( ( 900, 100 )
              , ( 1000, 100 )
              )
            , ( ( 900, 150 )
              , ( 1000, 150 )
              )
            ]
            Color.black
        , P.functionCurve (\x -> 100 * sin ((x - model.lasttime * 50) / 25)) ( 1000, 100 ) ( 0, 920 ) 0.2 Color.black
        , P.rect ( 600, 100 ) ( 20, 20 ) Color.black
        , P.rectCentered ( 650, 100 ) ( 20, 20 ) 0.1 Color.black
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            if model.loadednum < 1 then
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
                    let
                        w =
                            Result.withDefault 0 <| Decode.decodeValue (Decode.at [ "response", "width" ] Decode.float) x

                        h =
                            Result.withDefault 0 <| Decode.decodeValue (Decode.at [ "response", "height" ] Decode.float) x
                    in
                    ( { model | loadednum = model.loadednum + 1, ts = ( w, h ) }, Cmd.none )

                Ok "loadFont" ->
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
