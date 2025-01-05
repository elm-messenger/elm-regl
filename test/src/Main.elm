port module Main exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadTexture, render, startREGL, toHtmlWith, triangle)
import REGL.Common exposing (Renderable)
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
    { lasttime : Float
    , loadednum : Int
    , ts : ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lasttime = 0
      , loadednum = 0
      , ts = ( 0, 0 )
      }
    , Cmd.batch
        -- [ loadTexture ( "enemy", "asset/enemy.png" )
        -- , createREGLProgram <| ( "mytriangle", encodeProgram myTriangleProgram )
        -- , configREGL <| Encode.object [ ( "interval", Encode.float 0 ) ]
        -- -- , createGLProgram <| ( "triangle", encodeProgram Triangle.prog )
        -- ]
        (batchExec execREGLCmd
            [ loadTexture "enemy" "asset/enemy.png" Nothing

            -- , loadMSDFFont "firacode" "asset/fira.png" "asset/fira.json"
            , startREGL (REGLStartConfig 1920 1080 5)
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


genRenderable1 : Model -> Renderable
genRenderable1 model =
    let
        numx =
            50

        numy =
            30

        bgColor =
            Color.rgba 1 1 1 0

        redC =
            Color.rgba 1 0 0 0.5
    in
    REGL.group [] <|
        REGL.clear bgColor
            :: (List.concat <|
                    List.map
                        (\x ->
                            List.map
                                (\y ->
                                    -- mytriangle
                                    --     ( model.lasttime / 10 + toFloat x / numx - 1, toFloat y / numy - 1 )
                                    triangle ( model.lasttime * 30 + toFloat x / numx * 640, toFloat y / numy * 360 + 5 )
                                        ( model.lasttime * 30 + toFloat x / numx * 640 + 5, toFloat y / numy * 360 + 15 )
                                        ( model.lasttime * 30 + toFloat x / numx * 640 + 10, toFloat y / numy * 360 + 5 )
                                        redC
                                )
                                (List.range 0 (numy * 2))
                        )
                        (List.range 0 (numx * 2))
               )


lorem =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Vivamus tortor massa, hendrerit eu tellus non, iaculis cursus purus.
Phasellus at tempor ipsum.
Quisque efficitur tortor sed tincidunt elementum.
Aliquam erat volutpat.
Morbi eu diam a mauris venenatis tincidunt eu et diam.
In hac habitasse platea dictumst.
Curabitur vitae massa at justo pellentesque molestie nec a diam. Fusce sed neque neque.
Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.
Etiam ut augue gravida,
dictum felis in, semper nisi.
Vestibulum a odio quis neque lobortis luctus eget at orci."""


genRenderable2 : Model -> Renderable
genRenderable2 model =
    let
        ( w, h ) =
            model.ts
    in
    REGL.group []
        [ REGL.clear (Color.rgba 1 1 1 1)
        , REGL.textbox ( 0, 1050 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "arial"
        , REGL.quad ( 0, 0 ) ( 1920, 0 ) ( 1920 / 3, 1080 / 3 ) ( 0, 1080 ) (Color.rgba 1 0.2 0.4 1)
        , REGL.textbox ( 100, 960 ) 30 lorem "arial"
        , REGL.textbox ( 100, 500 ) 500 "[BIG]" "arial"
        , REGL.group [ REGL.blur 1 ]
            [ REGL.clear (Color.rgba 1 0.2 0.4 0)
            , REGL.triangle ( 700, 100 ) ( 700 + 100, 100 ) ( 700 + 100, 100 / 2 ) Color.red
            , REGL.triangle ( 500, 100 ) ( 500 + 100, 100 ) ( 500 + 100, 100 / 2 ) Color.green
            ]
        , REGL.poly
            [ ( 1100, 600 )
            , ( 1100, 650 )
            , ( 1200, 680 )
            , ( 1300, 650 )
            , ( 1200, 600 )
            ]
            Color.blue
        , REGL.texture ( 0, 0 ) ( 2 * w, 0 ) ( 2 * w, h ) ( 0, h ) "enemy"
        , REGL.centeredTexture ( 1400, 300 ) ( w, h ) (model.lasttime / 5) "enemy"
        , REGL.circle ( 1400, 300 ) 30 Color.black
        , REGL.group [ REGL.gblur 10 ]
            [ REGL.clear (Color.rgba 1 1 1 0)
            , REGL.quad ( 1500, 500 ) ( 1800, 500 ) ( 1800, 900 ) ( 1500, 900 ) (Color.rgba 0.4 0.7 0.9 1)
            ]
        , REGL.textbox ( 1510, 890 ) 30 "Hello\nThis is a clear text on\na blurred background." "arial"
        , REGL.lineloop
            [ ( 100, 100 )
            , ( 200, 100 )
            , ( 300, 200 )
            , ( 100, 200 )
            ]
            Color.black
        , REGL.lines
            [ ( ( 900, 100 )
              , ( 1000, 100 )
              )
            , ( ( 900, 150 )
              , ( 1000, 150 )
              )
            ]
            Color.black
        , REGL.functionCurve (\x -> 100 * sin ((x - model.lasttime * 50) / 25)) ( 1000, 100 ) ( 0, 920 ) 0.2 Color.black
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
                    [ setView <| render <| genRenderable2 model
                    ]
                )

        REGLRecv x ->
            let
                cmd =
                    Decode.decodeValue (Decode.at [ "cmd" ] Decode.string) x
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
