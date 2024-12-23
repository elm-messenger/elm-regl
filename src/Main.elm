port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Encode as Encode


port setView : Encode.Value -> Cmd msg


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lasttime = 0
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | TextureLoaded String


clear : List ( String, Encode.Value )
clear =
    [ ( "cmd", Encode.int 1 )
    , ( "name", Encode.string "clear" )
    , ( "args"
      , Encode.object
            [ ( "color", Encode.list Encode.float [ 0.0, 0.0, 0.0, 0.0 ] )
            , ( "depth", Encode.float 1.0 )
            ]
      )
    ]


renderTri : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> List ( String, Encode.Value )
renderTri ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) uTime =
    [ ( "cmd", Encode.int 0 )
    , ( "program", Encode.string "renderTriangle" )
    , ( "args"
      , Encode.object
            [ ( "x", Encode.list Encode.float [ x1, y1 ] )
            , ( "y", Encode.list Encode.float [ x2, y2 ] )
            , ( "z", Encode.list Encode.float [ x3, y3 ] )
            , ( "uTime", Encode.float uTime )
            ]
      )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            let
                numx =
                    50

                numy =
                    30

                delta =
                    t - model.lasttime
            in
            ( { model | lasttime = t }
            , Cmd.batch
                [ setView <|
                    Encode.list Encode.object
                        (clear
                            :: (List.concat <|
                                    List.map
                                        (\x ->
                                            List.map
                                                (\y ->
                                                    renderTri
                                                        ( t / 10 + toFloat x / numx - 1, toFloat y / numy - 1 )
                                                        ( t / 10 + toFloat x / numx - 1 + 0.01, toFloat y / numy - 1 + 0.03 )
                                                        ( t / 10 + toFloat x / numx - 1 + 0.02, toFloat y / numy - 1 )
                                                        (t + toFloat x + toFloat y)
                                                )
                                                (List.range 0 (numy * 2))
                                        )
                                        (List.range 0 (numx * 2))
                               )
                        )
                ]
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ reglupdate Tick
        ]



-- VIEW


view : Model -> Html Msg
view _ =
    toHtmlWith { width = 1264, height = 711 }
        [ style "left" "136px"
        , style "top" "0px"
        , style "position" "fixed"
        ]


toHtmlWith :
    { width : Int
    , height : Int
    }
    -> List (Attribute msg)
    -> Html msg
toHtmlWith options attrs =
    Keyed.node "elm-regl"
        attrs
        [ ( "__canvas", canvas [ height options.height, width options.width, id "elm-regl-canvas" ] [] ) ]
