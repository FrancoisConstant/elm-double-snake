module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, p, span, strong, table, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (range)
import Positions exposing (..)
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
    , appleToReplaceKey : AppleKey -- next apple replaced when generating a new one
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
    = NOT_STARTED
    | WIP
    | PAUSED
    | GAME_OVER


type Direction
    = UP
    | RIGHT
    | DOWN
    | LEFT


type Key
    = SPACE
    | DIRECTION Direction


init : flags -> ( Model, Cmd Msg )
init _ =
    ( getDefaultModel NOT_STARTED, Cmd.none )


{-| Model when starting OR re-starting the game
-}
getDefaultModel : Game -> Model
getDefaultModel game =
    { -- UI
      game = game
    , score = 0

    -- timer / animations
    , totalTime = 0
    , elapsedTimeSinceLastUpdate = 0

    -- snakes
    , snake =
        { positions = [ Position 5 5, Position 6 5 ]
        , directions = [ RIGHT ]
        }
    , otherSnake =
        { positions = [ Position 20 20, Position 21 20 ]
        , directions = [ RIGHT ]
        }

    -- apples
    , apples =
        Dict.fromList [ ( 1, Position 14 18 ), ( 2, Position 8 6 ) ]
    , appleToReplaceKey = 1
    }



--
-- SUBSCRIPTIONS
-- (new frames & new user inputs)
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
            ( { model | game = WIP }, Cmd.none )

        ButtonReStartClicked ->
            ( getDefaultModel WIP, Cmd.none )

        Frame time ->
            updateFrame time model

        KeyPushed key ->
            updateKeyPushed key model

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
            { snake | directions = removePreviousDirection snake.directions }

        newHeadPosition =
            getNewHeadPosition model.snake.positions (getDirection snake2)

        doesSnakeEat =
            List.member newHeadPosition (getApplePositions model)

        futureSnake =
            updateSnakePosition doesSnakeEat snake2

        otherSnakeHead =
            getNewHeadPosition model.otherSnake.positions (getDirection model.otherSnake)

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
            getNewOtherSnakeDirection otherSnakeHead (getDirection model.otherSnake) (getApplePositions model)

        otherSnake3 =
            { futureOtherSnake
                | directions = [ newOtherSnakeDirection ]
            }

        appleToReplaceKey =
            let
                apple1 =
                    Maybe.withDefault (Position -1 -1) (model.apples |> Dict.get 1)
            in
            if apple1 == newHeadPosition || apple1 == otherSnakeHead then
                1

            else
                2

        cmd =
            if (doesSnakeEat || doesOtherSnakeEat) && not doesCrash then
                generateRandomPosition NewApplePosition

            else
                Cmd.none
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
        -- keep playing, update Snakes' positions
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
        ( model, generateRandomPosition NewApplePosition )

    else
        ( model
            |> setApples
                (model.apples
                    |> Dict.update model.appleToReplaceKey (\_ -> Just newPosition)
                )
        , Cmd.none
        )


{-| On user input, either start the game; pause the game or handle the new
direction: add the direction to the snake-directions-buffer if possible
(snake cannot reverse direction)
-}
updateKeyPushed : Key -> Model -> ( Model, Cmd Msg )
updateKeyPushed key model =
    case key of
        DIRECTION newDirection ->
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

        SPACE ->
            case model.game of
                NOT_STARTED ->
                    ( { model | game = WIP }, Cmd.none )

                PAUSED ->
                    ( { model | game = WIP }, Cmd.none )

                WIP ->
                    ( { model | game = PAUSED }, Cmd.none )

                GAME_OVER ->
                    -- re-start
                    ( getDefaultModel WIP, Cmd.none )



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
    div [ class "menu" ]
        ((if model.game == NOT_STARTED then
            [ button
                [ onClick ButtonStartClicked, class "button button-start" ]
                [ text "Start" ]
            ]

          else if model.game == GAME_OVER then
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

          else
            [ span [] [] ]
         )
            ++ [ p
                    [ class "score" ]
                    [ text (model.score |> String.fromInt) ]
               ]
        )


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
                "snake-head"

            else if showSnake then
                "snake-body"

            else if showApple then
                "apple"

            else if showOtherSnakeHead then
                "snake-2-head"

            else if showOtherSnake then
                "snake-2-body"

            else if showShadow then
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
            applePositions |> List.head |> Maybe.withDefault { x = 1, y = 1 }

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


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map
        stringToKey
        (Decode.field "key" Decode.string)


stringToKey : String -> Key
stringToKey string =
    case string of
        "ArrowLeft" ->
            DIRECTION LEFT

        "ArrowRight" ->
            DIRECTION RIGHT

        "ArrowUp" ->
            DIRECTION UP

        "ArrowDown" ->
            DIRECTION DOWN

        " " ->
            SPACE

        _ ->
            DIRECTION RIGHT


{-| Making sure the player cannot reverse the direction.
-}
canUpdateDirection : Direction -> List Direction -> Bool
canUpdateDirection newDirection directions =
    let
        latestDirection =
            List.reverse directions |> List.head |> Maybe.withDefault LEFT
    in
    (newDirection /= latestDirection)
        && not (newDirection == UP && latestDirection == DOWN)
        && not (newDirection == DOWN && latestDirection == UP)
        && not (newDirection == LEFT && latestDirection == RIGHT)
        && not (newDirection == RIGHT && latestDirection == LEFT)


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
        |> Maybe.withDefault RIGHT


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
