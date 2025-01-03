port module Main exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, blur, genProg, loadMSDFFont, loadTexture, render, startREGL, toHtmlWith, triangle)
import REGL.Common exposing (Renderable)
import REGL.Compositors as Comp
import REGL.Program exposing (ProgValue(..), REGLProgram)
import String exposing (fromFloat, fromInt)


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
        -- [ loadTexture ( "enemy", "asset/enemy.png" )
        -- , createREGLProgram <| ( "mytriangle", encodeProgram myTriangleProgram )
        -- , configREGL <| Encode.object [ ( "interval", Encode.float 0 ) ]
        -- -- , createGLProgram <| ( "triangle", encodeProgram Triangle.prog )
        -- ]
        (batchExec execREGLCmd
            [ loadTexture "enemy" "asset/enemy.png" Nothing

            -- , loadMSDFFont "firacode" "asset/fira.png" "asset/fira.json"
            , startREGL (REGLStartConfig 1280 720)
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
    REGL.group <|
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


genRenderable2 : Model -> Renderable
genRenderable2 model =
    REGL.groupEffects []
        [ REGL.clear (Color.rgba 1 1 1 1)
        , REGL.simpText ("hello world\ni\nhihi blablablabl blablablabl blablabl:)" ++ fromInt (floor model.lasttime))
        , REGL.triangle ( 400, 300 ) ( 400 + 100, 300 ) ( 400 + 100, 300 / 2 ) Color.red
        , REGL.quad ( 0, 0 ) ( 1280, 0 ) ( 1280 / 3, 720 / 3 ) ( 0, 720 ) (Color.rgba 0.5 0.5 0.7 1)
        , REGL.circle ( 200, 100 ) 100 Color.lightBrown
        , REGL.groupEffects [ blur 2 ]
            [ REGL.clear (Color.rgba 0.5 0.5 0.7 0)
            , REGL.triangle ( 700, 100 ) ( 700 + 100, 100 ) ( 700 + 100, 100 / 2 ) Color.red
            , REGL.triangle ( 500, 100 ) ( 500 + 100, 100 ) ( 500 + 100, 100 / 2 ) Color.green
            ]

        -- , REGL.triangle ( 700, 150 ) ( 700 + 100, 150 ) ( 700 + 100, 150 / 2 ) (Color.rgba 0 0 0 1)
        -- , Comp.dstOverSrc
        --     (REGL.group
        --         [ REGL.triangle ( 500, 150 ) ( 500 + 100, 150 ) ( 500 + 100, 150 / 2 ) Color.green
        --         ]
        --     )
        --     (REGL.group
        --         [ REGL.triangle ( 550, 150 ) ( 550 + 100, 150 ) ( 550 + 100, 150 / 2 ) Color.red
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
                    ( { model | loadednum = model.loadednum + 1 }, Cmd.none )

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
