module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class, href, rel)
import List exposing (range)



--
-- MAIN
--


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--
-- MODEL
--


type alias Model =
    { snake : Snake
    }


type alias Snake =
    { positions : List Position
    , direction : Direction
    }


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = UP
    | RIGHT
    | DOWN
    | LEFT


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { snake =
            { positions =
                [ { x = 12, y = 10 }
                , { x = 11, y = 10 }
                , { x = 10, y = 10 }
                ]
            , direction = RIGHT
            }
      }
    , Cmd.none
    )


type Msg
    = Frame Float



--
-- SUBSCRIPTIONS
--


subscriptions _ =
    onAnimationFrameDelta Frame



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , table
            [ class "mt-24 ml-24" ]
            (renderRows model)
        ]


renderRows : Model -> List (Html Msg)
renderRows model =
    let
        y_list =
            range 0 40
    in
    y_list
        |> List.map
            (\y -> tr [] (renderColumns y model))


renderColumns : Int -> Model -> List (Html Msg)
renderColumns y model =
    let
        x_list =
            range 0 40
    in
    x_list
        |> List.map
            (\x -> renderCase { x = x, y = y } model)


renderCase : Position -> Model -> Html Msg
renderCase position model =
    let
        showSnake =
            isSnakeOn position model

        showSnakeHead =
            isSnakeHead position model
    in
    td
        [ if showSnakeHead then
            class "bg-blue-600 w-4 h-4"

          else if showSnake then
            class "bg-blue-400 w-4 h-4"

          else
            class "bg-gray-300 w-4 h-4"
        ]
        [ text " " ]



--
-- UTILS
--


isSnakeOn : Position -> Model -> Bool
isSnakeOn position model =
    List.member position model.snake.positions


isSnakeHead : Position -> Model -> Bool
isSnakeHead position model =
    position
        == ((model.snake.positions |> List.head) |> Maybe.withDefault { x = -1, y = -1 })


stylesheet : Html.Html msg
stylesheet =
    Html.node "link"
        [ rel "stylesheet"
        , href "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css"
        ]
        []
