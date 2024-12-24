port module Main exposing (..)

import Browser
import Color as EC
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import REGL exposing (genProg, render, toHtmlWith)
import REGL.API exposing (appendArgs, programBase, toRgbaList)
import REGL.Common exposing (Color(..), Renderable)
import REGL.Program exposing (ProgValue(..), REGLProgram, encodeProgram)
import REGL.Triangle as Triangle


port setView : Encode.Value -> Cmd msg


port loadTexture : ( String, String ) -> Cmd msg


port createREGLProgram : ( String, Encode.Value ) -> Cmd msg


port configREGL : Encode.Value -> Cmd msg


port textureLoaded : (Encode.Value -> msg) -> Sub msg


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


frag =
    """
precision mediump float;
uniform sampler2D texture;
varying vec2 uv;
void main() {
    gl_FragColor = texture2D(texture, uv);
}

"""


vert =
    """
precision mediump float;
attribute vec2 position;
attribute vec2 texc;
uniform vec2 offset;
varying vec2 uv;
void main() {
    uv = texc;
    gl_Position = vec4(-position + offset, 0, 1);
}

"""


myTriangleProgram : REGLProgram
myTriangleProgram =
    { frag = frag
    , vert = vert
    , attributes =
        Just
            [ ( "position"
              , StaticValue <|
                    Encode.list Encode.float
                        [ 0.02
                        , 0.02
                        , 0.02
                        , -0.02
                        , -0.02
                        , -0.02
                        , -0.02
                        , 0.02
                        ]
              )
            , ( "texc"
              , StaticValue <|
                    Encode.list Encode.float
                        [ 1
                        , 1
                        , 1
                        , 0
                        , 0
                        , 0
                        , 0
                        , 1
                        ]
              )
            ]
    , uniforms =
        Just
            [ ( "texture", DynamicTextureValue "texture" )
            , ( "offset", DynamicValue "offset" )
            ]
    , elements = Just [ 0, 1, 2, 0, 2, 3 ]
    , count = 6
    , primitive = Nothing
    }


mytriangleProgram : List ( String, Encode.Value )
mytriangleProgram =
    programBase "mytriangle"


mytriangle : ( Float, Float ) -> Renderable
mytriangle ( x1, y1 ) =
    genProg <|
        appendArgs
            [ ( "texture", Encode.string "enemy" )
            , ( "offset", Encode.list Encode.float [ x1, y1 ] )
            ]
            mytriangleProgram


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lasttime = 0
      , loadednum = 0
      }
    , Cmd.batch
        [ loadTexture ( "enemy", "asset/enemy.png" )
        , createREGLProgram <| ( "mytriangle", encodeProgram myTriangleProgram )
        , configREGL <| Encode.object [ ( "interval", Encode.float 0 ) ]

        -- , createGLProgram <| ( "triangle", encodeProgram Triangle.prog )
        ]
    )


toMColor : EC.Color -> Color
toMColor c =
    let
        cc =
            EC.toRgba c
    in
    ColorRGBA (cc.red * 255) (cc.green * 255) (cc.blue * 255) (cc.alpha * 255)


type Msg
    = Tick Float
    | TextureLoaded Encode.Value


genRenderable : Model -> Renderable
genRenderable model =
    let
        numx =
            50

        numy =
            25

        bgColor =
            toMColor <| EC.rgba 0 0 0 0

        redC =
            toMColor EC.red
    in
    REGL.group <|
        REGL.clear bgColor 1
            :: (List.concat <|
                    List.map
                        (\x ->
                            List.map
                                (\y ->
                                    -- mytriangle
                                    --     ( model.lasttime / 10 + toFloat x / numx - 1, toFloat y / numy - 1 )
                                    Triangle.triangle ( model.lasttime / 10 + toFloat x / numx - 1, toFloat y / numy - 1 )
                                        ( model.lasttime / 10 + toFloat x / numx - 1 + 0.01, toFloat y / numy - 1 + 0.03 )
                                        ( model.lasttime / 10 + toFloat x / numx - 1 + 0.02, toFloat y / numy - 1 )
                                        redC
                                )
                                (List.range 0 (numy * 2))
                        )
                        (List.range 0 (numx * 2))
               )


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

                    -- Encode.list Encode.object
                    --     ((List.concat <|
                    --                 List.map
                    --                     (\x ->
                    --                         List.map
                    --                             (\y ->
                    --                                 renderTexture
                    --                                     ( t / 10 + toFloat x / numx - 1, toFloat y / numy - 1 )
                    --                             )
                    --                             (List.range 0 (numy * 2))
                    --                     )
                    --                     (List.range 0 (numx * 2))
                    --            )
                    --     )
                    ]
                )

        TextureLoaded x ->
            let
                success =
                    Decode.decodeValue (Decode.at [ "success" ] Decode.bool) x

                ss =
                    Decode.decodeValue (Decode.at [ "texture" ] Decode.string) x
            in
            ( { model | loadednum = model.loadednum + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ reglupdate Tick
        , textureLoaded TextureLoaded
        ]


view : Model -> Html Msg
view _ =
    toHtmlWith { width = 1264, height = 711 }
        [ style "left" "136px"
        , style "top" "0px"
        , style "position" "fixed"
        ]
