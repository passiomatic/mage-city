module Vector3Extra exposing (zero, fromColor)

import Color exposing (Color)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)

zero =
    vec3 0 0 0


fromColor : Color -> Vec3
fromColor color =
    case Color.toRgb color of
        { red, green, blue } ->
            vec3 (toFloat red / 256) (toFloat green / 256) (toFloat blue / 256)
