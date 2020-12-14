module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, span, table, td, text, tr)
import Html.Attributes exposing (class, href, rel, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (range)
import Random
import Settings exposing (bottomLimit, leftLimit, rightLimit, sizeX, sizeY, topLimit)



--
-- MESSAGES
--


type Msg
    = Frame Float
    | ButtonStartClicked
    | KeyPushed Direction
    | NewApplePosition Position



--
-- MODEL
--


type alias Model =
    { game : Game
    , score : Score
    , snake : Snake
    , otherSnake : Snake
    , apples : Dict AppleKey Apple
    , appleToReplaceKey : AppleKey
    , totalTime : Float
    , elapsedTimeSinceLastUpdate : Float
    }


type alias Snake =
    { positions : List Position

    -- TODO: should use a buffer of directions
    , currentDirection : Direction
    , nextDirection : Direction
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Apple =
    Position


type alias AppleKey =
    Int


type alias Score =
    Int


type Game
    = NOT_STARTED
    | WIP
    | GAME_OVER


type Direction
    = UP
    | RIGHT
    | DOWN
    | LEFT


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { -- UI
        game = NOT_STARTED
      , score = 0

      -- timer / animations
      , totalTime = 0
      , elapsedTimeSinceLastUpdate = 0

      -- snakes and points
      , snake =
            { positions =
                [ { x = 5, y = 5 }
                , { x = 6, y = 5 }
                ]

            -- currentDirection is updated on each tick
            , currentDirection = RIGHT
            , nextDirection = RIGHT
            }
      , otherSnake =
            { positions =
                [ { x = 20, y = 20 }
                , { x = 21, y = 20 }
                ]

            -- currentDirection is updated on each tick
            , currentDirection = RIGHT
            , nextDirection = RIGHT
            }
      , apples =
            Dict.fromList
                [ ( 1, { x = 14, y = 18 } )
                , ( 2, { x = 8, y = 6 } )
                ]
      , appleToReplaceKey = 1
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

        ButtonStartClicked ->
            buttonStartClicked model

        KeyPushed direction ->
            keyPushed direction model

        NewApplePosition position ->
            updateApplePosition position model


updateFrame : Float -> Model -> ( Model, Cmd Msg )
updateFrame timeDelta model =
    if model.game == WIP then
        let
            elapsedTimeSinceLastUpdate =
                model.elapsedTimeSinceLastUpdate + timeDelta
        in
        if elapsedTimeSinceLastUpdate >= 100 then
            doUpdateFrame timeDelta model

        else
            updateTimesOnly timeDelta model

    else
        updateTimesOnly timeDelta model


updateTimesOnly : Float -> Model -> ( Model, Cmd Msg )
updateTimesOnly timeDelta model =
    ( model |> updateTimes timeDelta False, Cmd.none )


doUpdateFrame : Float -> Model -> ( Model, Cmd Msg )
doUpdateFrame timeDelta model =
    let
        snake =
            model.snake

        snake2 =
            { snake | currentDirection = snake.nextDirection }

        newHeadPosition =
            getNewHeadPosition model.snake.positions model.snake.nextDirection

        doesSnakeEat =
            List.member newHeadPosition (getApplePositions model)

        futureSnake =
            updateSnakePosition doesSnakeEat snake2

        otherSnakeHead =
            getNewHeadPosition model.otherSnake.positions model.otherSnake.nextDirection

        doesOtherSnakeEat =
            List.member otherSnakeHead (getApplePositions model)

        futureOtherSnake =
            updateSnakePosition doesOtherSnakeEat model.otherSnake

        doesCrash =
            isSnakeCrashing futureSnake model.otherSnake

        score =
            if doesSnakeEat && not doesCrash then
                model.score + 1

            else
                model.score

        newOtherSnakeDirection =
            getNewOtherSnakeDirection otherSnakeHead model.otherSnake.currentDirection (getApplePositions model)

        otherSnake3 =
            { futureOtherSnake
                | currentDirection = newOtherSnakeDirection
                , nextDirection = newOtherSnakeDirection
            }

        appleToReplaceKey =
            let
                apple1 =
                    Maybe.withDefault { x = -1, y = -1 } (model.apples |> Dict.get 1)
            in
            if apple1 == newHeadPosition || apple1 == otherSnakeHead then
                1

            else
                2

        cmd =
            if (doesSnakeEat || doesOtherSnakeEat) && not doesCrash then
                Random.generate NewApplePosition getRandomPosition

            else
                Cmd.none
    in
    if doesCrash then
        -- lost - don't update the position (so we still see the head)
        ( model
            |> setGameOver
            |> setScore score
            |> updateTimes timeDelta True
        , Cmd.none
        )

    else
        -- keep playing, update Snakes' position
        ( model
            |> setAppleToReplaceKey appleToReplaceKey
            |> setSnake futureSnake
            |> setOtherSnake otherSnake3
            |> setScore score
            |> updateTimes timeDelta True
        , cmd
        )


updateApplePosition : Position -> Model -> ( Model, Cmd Msg )
updateApplePosition newPosition model =
    let
        isOnAnySnake =
            List.member newPosition (model.snake.positions ++ model.otherSnake.positions)
    in
    if isOnAnySnake then
        ( model, Random.generate NewApplePosition getRandomPosition )

    else
        ( model
            |> setApples
                (model.apples
                    |> Dict.update model.appleToReplaceKey (\_ -> Just newPosition)
                )
        , Cmd.none
        )


buttonStartClicked : Model -> ( Model, Cmd Msg )
buttonStartClicked model =
    ( { model | game = WIP }, Cmd.none )


keyPushed : Direction -> Model -> ( Model, Cmd Msg )
keyPushed newDirection model =
    if canUpdateDirection newDirection model.snake.currentDirection then
        let
            snake =
                model.snake

            newSnake =
                { snake | nextDirection = newDirection }
        in
        ( newSnake |> asSnakeIn model, Cmd.none )

    else
        ( model, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    div [ class "min-w-full p-24 pt-36 bg-gray-100" ]
        [ stylesheet
        , div [ class "bg-whiter", style "width" "80%" ]
            [ table
                [ class "border-collapse bg-gradient-to-r from-blue-600 to-blue-300 float-left" ]
                (renderRows model)
            , viewMenu model
            ]
        , div
            [ class "clear-both" ]
            [ p [ class "pt-24 text-center text-gray-500" ]
                [ text
                    ("Use arrows to move. "
                        ++ "Avoid the walls. "
                        ++ "Avoid the black snake. "
                        ++ "Eat the apples."
                    )
                ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "float-left w-64 h-full min-h-full bg-white" ]
        ((if model.game == NOT_STARTED then
            [ button
                [ onClick ButtonStartClicked
                , class "mt-24 ml-24 bg-blue-300 pt-2 pr-4 pb-2 pl-4 text-white uppercase font-bold"
                ]
                [ text "Start" ]
            ]

          else
            [ span [] [] ]
         )
            ++ [ p
                    [ class "p-6 text-9xl text-right text-blue-300 mt-48" ]
                    [ text (model.score |> String.fromInt) ]
               ]
        )


renderRows : Model -> List (Html Msg)
renderRows model =
    let
        y_list =
            range 0 sizeY
    in
    y_list
        |> List.map
            (\y -> tr [] (renderColumns y model))


renderColumns : Int -> Model -> List (Html Msg)
renderColumns y model =
    let
        x_list =
            range 0 sizeX
    in
    x_list
        |> List.map
            (\x -> renderCase { x = x, y = y } model)


renderCase : Position -> Model -> Html Msg
renderCase position model =
    let
        showSnake =
            isSnakeOn position model.snake

        showSnakeHead =
            isSnakeHead position model.snake

        showApple =
            not showSnake && isAppleOn position model

        showOtherSnake =
            isSnakeOn position model.otherSnake

        showOtherSnakeHead =
            isSnakeHead position model.otherSnake

        -- shadow
        under =
            { position | y = position.y - 1 }

        showShadow =
            isSnakeOn under model.snake
                || isAppleOn under model
                || isSnakeOn under model.otherSnake

        color =
            if showSnakeHead then
                "bg-white"

            else if showSnake then
                "bg-gray-100"

            else if showApple then
                "bg-green-300"

            else if showOtherSnakeHead then
                "bg-black"

            else if showOtherSnake then
                "bg-gray-800"

            else if showShadow then
                "bg-blue-500"

            else
                ""
    in
    td
        [ class ("w-6 h-6 " ++ color) ]
        [ text " " ]



--
-- UTILS
--


asSnakeIn : Model -> Snake -> Model
asSnakeIn model snake =
    { model | snake = snake }


setSnake : Snake -> Model -> Model
setSnake snake model =
    { model | snake = snake }


setOtherSnake : Snake -> Model -> Model
setOtherSnake otherSnake model =
    { model | otherSnake = otherSnake }


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


updateSnakePosition : Bool -> Snake -> Snake
updateSnakePosition doesSnakeEat snake =
    { snake
        | positions = updatePositions snake.positions snake.currentDirection doesSnakeEat
    }


updatePositions : List Position -> Direction -> Bool -> List Position
updatePositions positions direction doesSnakeEat =
    let
        newHeadPosition =
            getNewHeadPosition positions direction
    in
    if doesSnakeEat then
        positions
            -- Add new head at the first position
            |> List.append [ newHeadPosition ]

    else
        positions
            -- Move latest position to the new head position:
            -- 1.Remove latest position
            |> List.reverse
            |> List.drop 1
            |> List.reverse
            -- 2.Add new head at the first position
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


isSnakeOn : Position -> Snake -> Bool
isSnakeOn position snake =
    List.member position snake.positions


isSnakeHead : Position -> Snake -> Bool
isSnakeHead position snake =
    position
        == ((snake.positions |> List.head) |> Maybe.withDefault { x = -1, y = -1 })


isAppleOn : Position -> Model -> Bool
isAppleOn position model =
    List.member position (getApplePositions model)


isSnakeCrashing : Snake -> Snake -> Bool
isSnakeCrashing snake otherSnake =
    let
        headPosition =
            snake.positions |> List.head |> Maybe.withDefault { x = -1, y = -1 }

        snakeBodyPositions =
            snake.positions |> List.drop 1

        _ =
            Debug.log "Pos" ( headPosition, snakeBodyPositions )
    in
    (-- check table sizes
     (headPosition.x > sizeX || headPosition.x < 0 || headPosition.y > sizeY || headPosition.y < 0)
        -- own body
        || List.member headPosition snakeBodyPositions
        -- enemy
        || List.any
            (\position -> List.member position otherSnake.positions)
            snake.positions
    )


closeToRight : Position -> Bool
closeToRight headPosition =
    headPosition.x > rightLimit


closeToLeft : Position -> Bool
closeToLeft headPosition =
    headPosition.x < leftLimit


closeToBottom : Position -> Bool
closeToBottom headPosition =
    headPosition.y > bottomLimit


closeToTop : Position -> Bool
closeToTop headPosition =
    headPosition.y < topLimit


getNewOtherSnakeDirection : Position -> Direction -> List Position -> Direction
getNewOtherSnakeDirection headPosition currentDirection applePositions =
    let
        applePosition =
            applePositions |> List.head |> Maybe.withDefault { x = 1, y = 1 }

        right =
            closeToRight headPosition

        left =
            closeToLeft headPosition

        top =
            closeToTop headPosition

        bottom =
            closeToBottom headPosition

        appleOnTheRight =
            applePosition.x > headPosition.x

        appleOnTheLeft =
            applePosition.x < headPosition.x

        appleAbove =
            applePosition.y < headPosition.y

        appleUnder =
            applePosition.y > headPosition.y
    in
    case currentDirection of
        -- logic to avoid walls OR get closer to the apple
        UP ->
            if top then
                if left then
                    RIGHT

                else
                    LEFT

            else if appleAbove then
                currentDirection

            else if appleOnTheRight then
                RIGHT

            else
                LEFT

        RIGHT ->
            if right then
                if top then
                    DOWN

                else
                    UP

            else if appleOnTheRight then
                currentDirection

            else if appleAbove then
                UP

            else
                DOWN

        DOWN ->
            if bottom then
                if right then
                    LEFT

                else
                    RIGHT

            else if appleUnder then
                currentDirection

            else if appleOnTheRight then
                RIGHT

            else
                LEFT

        LEFT ->
            if left then
                if bottom then
                    UP

                else
                    DOWN

            else if appleOnTheLeft then
                currentDirection

            else if appleAbove then
                UP

            else
                DOWN


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


setGameOver : Model -> Model
setGameOver model =
    { model | game = GAME_OVER }


setScore : Score -> Model -> Model
setScore score model =
    { model | score = score }


getApplePositions : Model -> List Position
getApplePositions model =
    model.apples |> Dict.values


setApples : Dict AppleKey Apple -> Model -> Model
setApples apples model =
    { model | apples = apples }


setAppleToReplaceKey : AppleKey -> Model -> Model
setAppleToReplaceKey appleToReplaceKey model =
    { model | appleToReplaceKey = appleToReplaceKey }


getRandomXPosition : Random.Generator Int
getRandomXPosition =
    Random.int 1 sizeX


getRandomYPosition : Random.Generator Int
getRandomYPosition =
    Random.int 1 sizeY


getRandomPosition : Random.Generator Position
getRandomPosition =
    Random.map2 Position getRandomXPosition getRandomYPosition



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
