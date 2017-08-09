module Vector2Extra exposing (zero, snap, fromInt, toDirection)

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2, vec2)


zero =
    vec2 0 0


fromInt : Int -> Int -> Vec2
fromInt x y =
    vec2 (toFloat x) (toFloat y)


{-| Snap value to nearest integer value
-}
snap : Vec2 -> Vec2
snap value =
    let
        (x, y) =
            Vector2.toTuple value
    in
        vec2 (rounder x) (rounder y)


rounder =
    floor >> toFloat


toDirection : Vec2 -> Direction
toDirection value =
    let
        (x, y) = value
            |> Vector2.normalize
            |> Vector2.toTuple

        isRight =
            x > 0.5

        isMiddleX =
            (x < 0.5) && (x > -0.5)

        isLeft =
            x < -0.5

        isTop =
            y > 0.5

        isMiddleY =
            (y < 0.5) && (y > -0.5)

        isBottom =
            y < -0.5

    in
        case (isLeft, isMiddleX, isRight, isTop, isMiddleY, isBottom) of
            ( False, True, False, True, False, False) ->
                North

            ( False, False, True, False, True, False ) ->
                East

            ( False, True, False, False, False, True)  ->
                South

            ( True, False, False, False, False, True)  ->
                South

            ( True, False, False, False, True, False ) ->
                West

            ( True, False, False, True, False, False ) ->
                West

            _ ->
                NoDirection
