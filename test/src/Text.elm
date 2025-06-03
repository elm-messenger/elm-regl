port module Text exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadTexture, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P exposing (defaultTextBoxOption)
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
            [ startREGL (REGLStartConfig 1920 1080 5 Nothing)
            ]
        )
    )


type Msg
    = Tick Float


lorem =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus tortor mass"""


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
        , P.rect ( 0, 0 ) ( 1000, 1080 ) (Color.rgba 1 0.5 0.5 1)
        , P.rect ( 0, 500 ) ( 500, 1080 ) (Color.rgba 0 1 0.5 1)

        -- , P.textboxPro ( 0, 700 ) (P.TextBoxOption "consolas" "hello world sma blabla nmnbmasdjfssasfdasdfkjk jsadl  ds\ts\tj" 70 Color.black False Nothing Nothing (Just 500) Nothing Nothing Nothing Nothing)
        , P.textbox ( 0, 0 ) 100 "hello\na\tb  c\n\n\ta aa\n---" "consolas" Color.black

        -- , P.textboxPro ( 0, 500 ) (P.TextBoxOption "consolas" lorem 70 (Color.rgba 0.5 0.5 1.0 0.5) False (Just 1) (Just 4) (Just 1000) Nothing Nothing Nothing Nothing)
        , P.textboxPro ( 0, 500 ) { defaultTextBoxOption | fonts = [ "consolas" ], text = lorem, size = 24 }

        -- , P.textboxPro ( 500, 0 ) (P.TextBoxOption "consolas" lorem 50 (Color.rgba 0.5 0.5 1.0 0.5) True Nothing Nothing (Just 500) Nothing Nothing Nothing Nothing)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ reglupdate Tick
        ]


view : Model -> Html Msg
view _ =
    toHtmlWith { width = 1280, height = 720 }
        [ style "left" "0px"
        , style "top" "0px"
        , style "position" "fixed"
        ]
