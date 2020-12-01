module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class, href, rel)
import Json.Decode as Decode
import List exposing (range)



--
-- MESSAGES
--


type Msg
    = Frame Float
    | KeyPushed Direction



--
-- MODEL
--


type alias Model =
    { snake : Snake
    , totalTime : Float
    , elapsedTimeSinceLastUpdate : Float
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
    ( { totalTime = 0
      , elapsedTimeSinceLastUpdate = 0
      , snake =
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



--
-- SUBSCRIPTIONS
--


subscriptions _ =
    Sub.batch <|
        [ Browser.Events.onKeyDown (Decode.map KeyPushed keyDecoder)
        , Browser.Events.onAnimationFrameDelta Frame
        ]



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame time ->
            updateFrame time model

        KeyPushed direction ->
            keyPushed direction model


updateFrame : Float -> Model -> ( Model, Cmd Msg )
updateFrame timeDelta model =
    let
        elapsedTimeSinceLastUpdate =
            model.elapsedTimeSinceLastUpdate + timeDelta
    in
    if elapsedTimeSinceLastUpdate >= 100 then
        doUpdateFrame timeDelta model

    else
        updateTimesOnly timeDelta model


updateTimesOnly : Float -> Model -> ( Model, Cmd Msg )
updateTimesOnly timeDelta model =
    ( model |> updateTimes timeDelta False, Cmd.none )


doUpdateFrame : Float -> Model -> ( Model, Cmd Msg )
doUpdateFrame timeDelta model =
    ( model.snake
        |> updateSnakePosition
        |> asSnakeIn model
        |> updateTimes timeDelta True
    , Cmd.none
    )


keyPushed : Direction -> Model -> ( Model, Cmd Msg )
keyPushed newDirection model =
    if canUpdateDirection newDirection model.snake.direction then
        let
            snake =
                model.snake

            newSnake =
                { snake | direction = newDirection }
        in
        ( newSnake |> asSnakeIn model, Cmd.none )

    else
        ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , table
            [ class "mt-24 ml-24 border-collapse" ]
            (renderRows model)
        ]


renderRows : Model -> List (Html Msg)
renderRows model =
    let
        y_list =
            range 0 30
    in
    y_list
        |> List.map
            (\y -> tr [] (renderColumns y model))


renderColumns : Int -> Model -> List (Html Msg)
renderColumns y model =
    let
        x_list =
            range 0 30
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

        color =
            if showSnakeHead then
                "bg-blue-600"

            else if showSnake then
                "bg-blue-400"

            else
                "bg-gray-300"
    in
    td
        [ class ("w-6 h-6 border-solid border-2 border-light-blue-500 " ++ color) ]
        [ text " " ]



--
-- UTILS
--


asSnakeIn : Model -> Snake -> Model
asSnakeIn model snake =
    { model | snake = snake }


updateTimes : Float -> Bool -> Model -> Model
updateTimes deltaTime newFrame model =
    let
        newTotalTime =
            model.totalTime + deltaTime

        newElapsedTimeSinceLastUpdate =
            if newFrame then
                0

            else
                model.elapsedTimeSinceLastUpdate + deltaTime
    in
    { model
        | totalTime = newTotalTime
        , elapsedTimeSinceLastUpdate = newElapsedTimeSinceLastUpdate
    }


updateSnakePosition : Snake -> Snake
updateSnakePosition snake =
    { snake
        | positions = updatePositions snake.positions snake.direction
    }


updatePositions : List Position -> Direction -> List Position
updatePositions positions direction =
    let
        newHeadPosition =
            getNewHeadPosition positions direction
    in
    positions
        -- Remove latest position
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        -- Add new head at the first position
        |> List.append [ newHeadPosition ]


getNewHeadPosition : List Position -> Direction -> Position
getNewHeadPosition positions direction =
    let
        maybeSnakeHead =
            positions |> List.head
    in
    case maybeSnakeHead of
        Nothing ->
            { x = 0, y = 0 }

        Just position ->
            -- 0, 0 in top-left corner
            case direction of
                UP ->
                    { position | y = position.y - 1 }

                RIGHT ->
                    { position | x = position.x + 1 }

                DOWN ->
                    { position | y = position.y + 1 }

                LEFT ->
                    { position | x = position.x - 1 }


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


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    let
        _ =
            Debug.log "a" string
    in
    case string of
        "ArrowLeft" ->
            LEFT

        "ArrowRight" ->
            RIGHT

        "ArrowUp" ->
            UP

        "ArrowDown" ->
            DOWN

        _ ->
            RIGHT


canUpdateDirection : Direction -> Direction -> Bool
canUpdateDirection newDirection currentDirection =
    -- Make sure the player cannot reverse the direction
    (newDirection /= currentDirection)
        && not (newDirection == UP && currentDirection == DOWN)
        && not (newDirection == DOWN && currentDirection == UP)
        && not (newDirection == LEFT && currentDirection == RIGHT)
        && not (newDirection == RIGHT && currentDirection == LEFT)



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
