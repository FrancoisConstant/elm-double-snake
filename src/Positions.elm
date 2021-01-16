module Positions exposing
    ( Position
    , getRandomPositionsNotIn
    , isAbove
    , isCloseToBottom
    , isCloseToLeft
    , isCloseToRight
    , isCloseToTop
    , isOnLeft
    , isOnRight
    , isUnder
    , movePositions
    )

import Random
import Settings exposing (bottomLimit, leftLimit, rightLimit, sizeX, sizeY, topLimit)



--
-- MODEL
--


{-| 0 0 in top-left corner
-}
type alias Position =
    { x : Int
    , y : Int
    }



--
-- UTILS (public)
--


isCloseToRight : Position -> Bool
isCloseToRight headPosition =
    headPosition.x > rightLimit


isCloseToLeft : Position -> Bool
isCloseToLeft headPosition =
    headPosition.x < leftLimit


isCloseToBottom : Position -> Bool
isCloseToBottom headPosition =
    headPosition.y > bottomLimit


isCloseToTop : Position -> Bool
isCloseToTop headPosition =
    headPosition.y < topLimit


isOnRight : Position -> Position -> Bool
isOnRight pos1 pos2 =
    pos1.x > pos2.x


isOnLeft : Position -> Position -> Bool
isOnLeft pos1 pos2 =
    pos1.x < pos2.x


isAbove : Position -> Position -> Bool
isAbove pos1 pos2 =
    pos1.y < pos2.y


isUnder : Position -> Position -> Bool
isUnder pos1 pos2 =
    pos1.y > pos2.y


{-| Move a list of positions by:

  - adding the new position as the new "head" position
  - OR by moving the latest position at the new "head" position

-}
movePositions : List Position -> Position -> Bool -> List Position
movePositions positions newFirstPosition addPosition =
    if addPosition then
        -- Add new position as the first one
        newFirstPosition :: positions

    else
        -- Move latest position to the new position
        newFirstPosition
            :: (positions
                    |> List.reverse
                    |> List.drop 1
                    |> List.reverse
               )


getRandomPositionsNotIn : Random.Seed -> Int -> List Position -> List Position
getRandomPositionsNotIn seed0 numberOfPositions excludePositions =
    if numberOfPositions == 1 then
        let
            ( position, seed1 ) =
                getRandomPositionNotIn seed0 excludePositions
        in
        [ position ]

    else if numberOfPositions == 2 then
        let
            ( position1, seed1 ) =
                getRandomPositionNotIn seed0 excludePositions

            ( position2, seed2 ) =
                getRandomPositionNotIn seed1 excludePositions
        in
        [ position1, position2 ]

    else
        []



--
-- UTILS (private)
--


getRandomPositionNotIn : Random.Seed -> List Position -> ( Position, Random.Seed )
getRandomPositionNotIn seed0 excludePositions =
    let
        ( position, seed1 ) =
            getRandomPosition seed0
    in
    if List.member position excludePositions then
        getRandomPositionNotIn seed1 excludePositions

    else
        ( position, seed1 )


getRandomPosition : Random.Seed -> ( Position, Random.Seed )
getRandomPosition seed0 =
    let
        ( x, seed1 ) =
            Random.step getRandomXPosition seed0

        ( y, seed2 ) =
            Random.step getRandomYPosition seed1
    in
    ( Position x y, seed2 )


getRandomXPosition : Random.Generator Int
getRandomXPosition =
    Random.int 1 sizeX


getRandomYPosition : Random.Generator Int
getRandomYPosition =
    Random.int 1 sizeY
