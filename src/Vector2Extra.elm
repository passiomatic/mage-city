module Vector2Extra exposing (zero, fromInt, toDirection)

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2, vec2)


zero =
    vec2 0 0


fromInt : Int -> Int -> Vec2
fromInt x y =
    vec2 (toFloat x) (toFloat y)


toDirection : Vec2 -> Direction
toDirection value =
    let
        newValue = value
            |> Vector2.normalize
            |> Vector2.toTuple
    in
    case newValue of
        ( 0, 1 ) ->
            North

        ( 1, 1 ) ->
            NorthEast

        ( 1, 0 ) ->
            East

        ( 1, -1 ) ->
            SouthEast

        ( 0, -1 ) ->
            South

        ( -1, -1 ) ->
            SouthWest

        ( -1, 0 ) ->
            West

        ( -1, 1 ) ->
            NorthWest

        _ ->
            NoDirection
