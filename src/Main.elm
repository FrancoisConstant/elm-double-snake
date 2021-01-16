module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, p, strong, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (range)
import Positions exposing (..)
import Random
import Settings exposing (sizeX, sizeY)



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
-- MESSAGES
--


type Msg
    = Frame Float
    | ButtonStartClicked
    | ButtonReStartClicked
    | KeyPushed Key



--
-- MODEL
--


type alias Model =
    { game : Game
    , score : Score
    , snake : Snake
    , otherSnake : Snake
    , apples : List Apple
    , totalTime : Float
    , elapsedTimeSinceLastUpdate : Float
    }


type alias Snake =
    { positions : List Position
    , directions : List Direction
    }


type alias Apple =
    Position


type alias AppleKey =
    Int


type alias Score =
    Int


type Game
    = NotStarted
    | Playing
    | Paused
    | GameOver


type Direction
    = Up
    | Right
    | Down
    | Left


type Key
    = Space
    | Arrow Direction


init : flags -> ( Model, Cmd Msg )
init _ =
    ( getModelForGameStart NotStarted, Cmd.none )


getModelForGameStart : Game -> Model
getModelForGameStart game =
    { -- UI
      game = game
    , score = 0

    -- timer / animations
    , totalTime = 0
    , elapsedTimeSinceLastUpdate = 0

    -- snakes
    , snake =
        { positions = [ Position 5 5, Position 6 5 ]
        , directions = [ Right ]
        }
    , otherSnake =
        { positions = [ Position 20 20, Position 21 20 ]
        , directions = [ Right ]
        }

    -- apples
    , apples = [ Position 10 6, Position 30 22 ]
    }



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
        ButtonStartClicked ->
            ( { model | game = Playing }, Cmd.none )

        ButtonReStartClicked ->
            ( getModelForGameStart Playing, Cmd.none )

        Frame time ->
            updateFrame time model

        KeyPushed key ->
            updateKeyPushed key model


updateFrame : Float -> Model -> ( Model, Cmd Msg )
updateFrame timeDelta model =
    if model.game == Playing then
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
            { snake | directions = removePreviousDirection snake.directions }

        newHeadPosition =
            getNewHeadPosition model.snake.positions (getDirection snake2)

        doesSnakeEat =
            List.member newHeadPosition model.apples

        futureSnake =
            updateSnakePosition doesSnakeEat snake2

        otherSnakeHead =
            getNewHeadPosition model.otherSnake.positions (getDirection model.otherSnake)

        doesOtherSnakeEat =
            List.member otherSnakeHead model.apples

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
            getNewOtherSnakeDirection otherSnakeHead (getDirection model.otherSnake) model.apples

        otherSnake3 =
            { futureOtherSnake
                | directions = [ newOtherSnakeDirection ]
            }
    in
    if doesCrash then
        -- lost - don't update the position
        -- (so we still see the head before it crashes)
        ( model
            |> setGameOver
            |> setScore score
            |> updateTimes timeDelta True
        , Cmd.none
        )

    else
        let
            applesLeft =
                removeEatenApples [ newHeadPosition, otherSnakeHead ] model.apples

            excludePositions =
                model.snake.positions ++ model.otherSnake.positions

            newApples =
                getRandomPositionsNotIn
                    (Random.initialSeed (model.totalTime |> round))
                    (2 - List.length applesLeft)
                    excludePositions

            apples =
                applesLeft ++ newApples
        in
        -- keep playing, update Snakes' positions
        ( model
            |> setApples apples
            |> setSnake futureSnake
            |> setOtherSnake otherSnake3
            |> setScore score
            |> updateTimes timeDelta True
        , Cmd.none
        )


{-| On user input, either start the game; pause the game or handle the new
direction: add the direction to the snake-directions-buffer if possible
(snake cannot reverse direction)
-}
updateKeyPushed : Key -> Model -> ( Model, Cmd Msg )
updateKeyPushed key model =
    case key of
        Arrow newDirection ->
            if canUpdateDirection newDirection model.snake.directions then
                let
                    snake =
                        model.snake

                    newSnake =
                        { snake | directions = snake.directions ++ [ newDirection ] }
                in
                ( newSnake |> asSnakeIn model, Cmd.none )

            else
                ( model, Cmd.none )

        Space ->
            case model.game of
                NotStarted ->
                    ( { model | game = Playing }, Cmd.none )

                Paused ->
                    ( { model | game = Playing }, Cmd.none )

                Playing ->
                    ( { model | game = Paused }, Cmd.none )

                GameOver ->
                    -- re-start
                    ( getModelForGameStart Playing, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "main" ]
            [ viewMenu model
            , table [ class "snake-table" ] (viewRows model)
            ]
        , div
            [ class "info" ]
            [ p [] [ strong [] [ text "Press space or click \"Start\" to start." ] ]
            , p [] [ text "Use arrows to move." ]
            , p [] [ text "Avoid the walls. Avoid the black snake." ]
            , p [] [ text "Eat the apples." ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    case model.game of
        NotStarted ->
            div [ class "menu" ]
                [ button
                    [ onClick ButtonStartClicked, class "button button-start" ]
                    [ text "Start" ]
                ]

        GameOver ->
            div [ class "menu" ]
                [ p [ class "dead" ] [ text "Dead !" ]
                , p [ class "dead-score" ]
                    [ text "You have "
                    , strong [] [ text (model.score |> String.fromInt) ]
                    , text " point(s)"
                    ]
                , button
                    [ onClick ButtonReStartClicked, class "button button-restart" ]
                    [ text "Re-start" ]
                ]

        _ ->
            -- Playing, Paused
            div [ class "menu" ]
                [ p
                    [ class "score" ]
                    [ text (model.score |> String.fromInt) ]
                ]


viewRows : Model -> List (Html Msg)
viewRows model =
    let
        y_list =
            range 0 sizeY
    in
    y_list
        |> List.map
            (\y -> tr [] (viewColumns y model))


viewColumns : Int -> Model -> List (Html Msg)
viewColumns y model =
    let
        x_list =
            range 0 sizeX
    in
    x_list
        |> List.map
            (\x -> viewCell { x = x, y = y } model)


viewCell : Position -> Model -> Html Msg
viewCell position model =
    let
        under =
            { position | y = position.y - 1 }

        color =
            if isSnakeHead position model.snake then
                "snake-head"

            else if isSnakeOn position model.snake then
                "snake-body"

            else if isAppleOn position model then
                "apple"

            else if isSnakeHead position model.otherSnake then
                "snake-2-head"

            else if isSnakeOn position model.otherSnake then
                "snake-2-body"

            else if
                isSnakeOn under model.snake
                    || isAppleOn under model
                    || isSnakeOn under model.otherSnake
            then
                "shadow"

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
    let
        newHeadPosition =
            getNewHeadPosition snake.positions (getDirection snake)

        positions =
            movePositions snake.positions newHeadPosition doesSnakeEat
    in
    { snake | positions = positions }


getNewHeadPosition : List Position -> Direction -> Position
getNewHeadPosition positions direction =
    let
        maybeSnakeHead =
            positions |> List.head
    in
    case maybeSnakeHead of
        Nothing ->
            Position 0 0

        Just position ->
            -- 0, 0 in top-left corner
            case direction of
                Up ->
                    { position | y = position.y - 1 }

                Right ->
                    { position | x = position.x + 1 }

                Down ->
                    { position | y = position.y + 1 }

                Left ->
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
    List.member position model.apples


isSnakeCrashing : Snake -> Snake -> Bool
isSnakeCrashing snake otherSnake =
    let
        headPosition =
            snake.positions |> List.head |> Maybe.withDefault { x = -1, y = -1 }

        snakeBodyPositions =
            snake.positions |> List.drop 1
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


getNewOtherSnakeDirection : Position -> Direction -> List Position -> Direction
getNewOtherSnakeDirection headPosition currentDirection applePositions =
    let
        applePosition =
            applePositions |> List.head |> Maybe.withDefault (Position 1 1)

        right =
            isCloseToRight headPosition

        left =
            isCloseToLeft headPosition

        top =
            isCloseToTop headPosition

        bottom =
            isCloseToBottom headPosition

        appleOnTheRight =
            isOnRight applePosition headPosition

        appleOnTheLeft =
            isOnLeft applePosition headPosition

        appleAbove =
            isAbove applePosition headPosition

        appleUnder =
            isUnder applePosition headPosition
    in
    case currentDirection of
        -- logic to avoid walls OR to get closer to the apple
        Up ->
            if top then
                if left then
                    Right

                else
                    Left

            else if appleAbove then
                currentDirection

            else if appleOnTheRight then
                Right

            else
                Left

        Right ->
            if right then
                if top then
                    Down

                else
                    Up

            else if appleOnTheRight then
                currentDirection

            else if appleAbove then
                Up

            else
                Down

        Down ->
            if bottom then
                if right then
                    Left

                else
                    Right

            else if appleUnder then
                currentDirection

            else if appleOnTheRight then
                Right

            else
                Left

        Left ->
            if left then
                if bottom then
                    Up

                else
                    Down

            else if appleOnTheLeft then
                currentDirection

            else if appleAbove then
                Up

            else
                Down


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map
        stringToKey
        (Decode.field "key" Decode.string)


stringToKey : String -> Key
stringToKey string =
    case string of
        "ArrowLeft" ->
            Arrow Left

        "ArrowRight" ->
            Arrow Right

        "ArrowUp" ->
            Arrow Up

        "ArrowDown" ->
            Arrow Down

        " " ->
            Space

        _ ->
            Arrow Right


{-| Making sure that the player:

  - is actually changing the direction
  - isn't trying to reverse it

-}
canUpdateDirection : Direction -> List Direction -> Bool
canUpdateDirection newDirection directions =
    let
        latestDirection =
            List.reverse directions |> List.head |> Maybe.withDefault Left
    in
    (newDirection /= latestDirection)
        && not (isOppositeDirection newDirection latestDirection)


isOppositeDirection : Direction -> Direction -> Bool
isOppositeDirection direction1 direction2 =
    List.member
        ( direction1, direction2 )
        [ ( Up, Down )
        , ( Down, Up )
        , ( Left, Right )
        , ( Right, Left )
        ]


{-| First direction is the oldest one.
Remove it if there are other directions in the buffer.
Keep it if not.
-}
removePreviousDirection : List Direction -> List Direction
removePreviousDirection directions =
    if List.length directions > 1 then
        List.drop 1 directions

    else
        directions


getDirection : Snake -> Direction
getDirection snake =
    snake.directions
        |> List.head
        |> Maybe.withDefault Right


setGameOver : Model -> Model
setGameOver model =
    { model | game = GameOver }


setScore : Score -> Model -> Model
setScore score model =
    { model | score = score }


setApples : List Apple -> Model -> Model
setApples apples model =
    { model | apples = apples }


removeEatenApples : List Position -> List Apple -> List Apple
removeEatenApples headPositions apples =
    apples |> List.filter (\apple -> not (List.member apple headPositions))
