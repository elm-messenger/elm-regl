port module Text exposing (..)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (REGLStartConfig, batchExec, loadMSDFFont, startREGL, toHtmlWith)
import REGL.BuiltinPrograms as P exposing (defaultTextBoxOption)
import REGL.Common exposing (Renderable, group, render)


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
            , loadMSDFFont "Consolas" "assets/Combined.png" "assets/Consolas.json"
            , loadMSDFFont "mono" "assets/Combined.png" "assets/Monospace.json"
            ]
        )
    )


type Msg
    = Tick Float
    | REGLRecv Encode.Value


lorem =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus tortor mass"""


genRenderable : Model -> Renderable
genRenderable _ =
    group []
        -- group (E.blur 3 ++ E.blur 2 ++ E.blur 1)
        [ P.clear (Color.rgba 1 1 1 1)
        , P.rect ( 0, 0 ) ( 1000, 1080 ) (Color.rgba 1 0.5 0.5 1)
        , P.rect ( 0, 500 ) ( 500, 1080 ) (Color.rgba 0 1 0.5 1)
        , P.textboxPro ( 0, 700 ) { defaultTextBoxOption | fonts = [ "consolas" ], text = "hello world sma blabla nmnbmasdjfssasfdasdfkjk jsadl  ds\ts\tj", size = 70, color = Color.black, width = Just 500 }
        , P.textbox ( 0, 0 ) 100 "hello\na\tb  c\n\n\ta aa\n---" "consolas" Color.black
        , P.textboxMF ( 0, 200 ) 100 "abcde↻ xyz" [ "Consolas", "mono" ] Color.black
        , P.textboxPro ( 0, 500 ) { defaultTextBoxOption | fonts = [ "consolas" ], text = lorem, size = 70, color = Color.rgba 0.5 0.5 0.5 0.5, width = Just 1000 }
        , P.textboxPro ( 500, 0 ) { defaultTextBoxOption | fonts = [ "consolas" ], text = lorem, size = 50, color = Color.rgba 0.5 0.5 0.5 0.5, wordBreak = True, width = Just 500 }
        , P.rectCentered ( 1200, 300 ) ( 20, 20 ) 0 (Color.rgba 1 0.5 0.5 1)
        , P.textboxPro ( 1200, 300 ) { defaultTextBoxOption | fonts = [ "consolas" ], text = lorem, size = 50, color = Color.black, width = Just 500, align = Just "center", valign = Just "center" }
        , P.rectCentered ( 1200, 500 ) ( 20, 20 ) 0 (Color.rgba 1 0.5 0.5 1)
        , P.textboxCentered ( 1200, 500 ) 100 "hello world" "consolas" Color.black
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
