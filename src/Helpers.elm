module Helpers exposing (..)

import Color exposing (Color)

import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Vector4 as Vector4 exposing (Vec4, vec4)

-- TODO remove
type alias Float2 =
    ( Float, Float )


type alias Int2 =
    ( Int, Int )


type alias Float3 =
    ( Float, Float, Float )


midpoint : Vec2 -> Vec2 -> Vec2
midpoint position size =
    Vector2.add position (Vector2.scale 0.5 size)


toVec3 : Vec2 -> Float -> Vec3
toVec3 value z =
    vec3 (Vector2.getX value) (Vector2.getY value) z



colorToVec3 : Color -> Vec3
colorToVec3 color =
    case Color.toRgb color of
        { red, green, blue } ->
            vec3 (toFloat red / 256) (toFloat green / 256) (toFloat blue / 256)


colorToVec4 : Color -> Vec4
colorToVec4 color =
    case Color.toRgb color of
        { red, green, blue, alpha } ->
            vec4 (toFloat red / 256) (toFloat green / 256) (toFloat blue / 256) (alpha / 256)
