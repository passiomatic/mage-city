module Vector3Extra exposing (zero, fromInt, fromVec2, fromColor)

import Color exposing (Color)

import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)

zero =
    vec3 0 0 0

fromInt : Int -> Int -> Vec2
fromInt x y =
    vec2 (toFloat x) (toFloat y)


fromVec2 : Vec2 -> Float -> Vec3
fromVec2 value z =
    vec3 (Vector2.getX value) (Vector2.getY value) z


fromColor : Color -> Vec3
fromColor color =
    case Color.toRgb color of
        { red, green, blue } ->
            vec3 (toFloat red / 256) (toFloat green / 256) (toFloat blue / 256)
