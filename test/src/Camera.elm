port module Camera exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadTexture, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P
import REGL.Common exposing (Camera, Renderable, group, groupWithCamera, render)
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
    in
    groupWithCamera (Camera 960 540 1 0)
        -- Global camera
        []
        [ P.clear (Color.rgba 1 0 1 1)
        , P.textbox ( 0, 0 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
        , groupWithCamera (Camera (100 + model.lasttime * 10) 840 1 0)
            []
            [ P.textbox ( 100, 500 ) 100 "[BIG]" "consolas" Color.black
            , P.textbox ( 100, 1000 ) 20 "[small]" "consolas" Color.black
            , P.textbox ( 100, 1020 ) 30 "[medium]" "consolas" Color.black
            ]
        , P.textbox ( 0, 200 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
        , groupWithCamera (Camera (960 - model.lasttime * 10) 540 1 0)
            []
            [ P.textbox ( 0, 300 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
            , group [ E.blur 1 ]
                [ P.clear (Color.rgba 1 0 1 0)
                , P.triangle ( 700, 100 ) ( 700 + 100, 100 ) ( 700 + 100, 100 / 2 ) Color.red
                , groupWithCamera (Camera 960 540 1 0)
                    []
                    [ P.triangle ( 500, 100 ) ( 500 + 100, 100 ) ( 500 + 100, 100 / 2 ) Color.green
                    ]
                ]
            , P.textbox ( 0, 100 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
            ]
        , P.textbox ( 500, 0 ) 100 ("hello :)" ++ fromInt (floor model.lasttime)) "consolas" Color.black
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
