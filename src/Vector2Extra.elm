module Vector2Extra exposing (zero, fromInt)

import Math.Vector2 as Vector2 exposing (Vec2, vec2)

fromInt : Int -> Int -> Vec2
fromInt x y =
    vec2 (toFloat x) (toFloat y)

zero =
    vec2 0 0
