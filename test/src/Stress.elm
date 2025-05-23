port module Stress exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadTexture, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P
import REGL.Common exposing (Camera, Renderable, group, groupWithCamera, render)


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { starttime = 0
      , lasttime = 0
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


genRenderable : Model -> Renderable
genRenderable model =
    let
        numx =
            50

        numy =
            30

        bgColor =
            Color.rgba 1 1 1 1

        redC =
            Color.rgba 1 0 0 0.5
    in
    group [] <|
        P.clear bgColor
            :: (List.concat <|
                    List.map
                        (\x ->
                            List.map
                                (\y ->
                                    P.triangle ( model.lasttime * 30 + toFloat x / numx * 1920, toFloat y / numy * 1000 + 15 )
                                        ( model.lasttime * 30 + toFloat x / numx * 1920 + 15, toFloat y / numy * 1000 + 45 )
                                        ( model.lasttime * 30 + toFloat x / numx * 1920 + 30, toFloat y / numy * 1000 + 15 )
                                        redC
                                 -- P.centeredTextureCropped ( model.lasttime * 30 + toFloat x / numx * 1920, toFloat y / numy * 1000 + 20 ) ( 20, 20 ) 0 ( 0, 0 ) ( 0.5, 1 ) "enemy"
                                 -- P.centeredTexture ( model.lasttime * 30 + toFloat x / numx * 1920, toFloat y / numy * 1000 + 20 ) ( 20, 20 ) 0 "enemy"
                                 -- P.rectTexture ( model.lasttime * 30 + toFloat x / numx * 1920, toFloat y / numy * 1000 + 20 ) ( 20, 20 ) 1.0 "enemy"
                                 -- P.rectTextureCropped ( model.lasttime * 30 + toFloat x / numx * 1920, toFloat y / numy * 1000 + 20 ) ( 20, 20 ) ( 0, 0 ) ( 0.5, 1 ) "enemy"
                                )
                                (List.range 0 (numy * 2))
                        )
                        (List.range 0 (numx * 2))
               )


genSimple : Renderable
genSimple =
    let
        bgColor =
            Color.rgba 0 0 0 1

        redC =
            Color.rgba 1 0 0 0.5
    in
    group []
        [ P.clear bgColor
        , P.triangle ( 0, 0 ) ( 100, 0 ) ( 50, 100 ) redC
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
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
