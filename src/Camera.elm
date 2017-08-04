module Camera exposing (Camera, fixedArea, getViewSize, view, moveBy, moveTo, follow)

{-| This provides a basic camera
-}

import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Matrix4 as Matrix4 exposing (Mat4)

import Vector2Extra as Vector2 exposing (fromInt)
import Helpers as Helpers exposing (..)


{-|
A camera represents how to render the virtual world. It's essentially a
transformation from virtual game coordinates to pixel coordinates on the screen
-}
type alias Camera =
    { area : Float
    , position : Vec2
    }


{-|
A camera that always shows the same viewport area. This is useful in a top down game.
This means that you probably want to specify the area property like this:

    fixedArea (16, 10) (x, y)

This would show 16 by 10 units _if_ the game is displayed in a 16:10 viewport. However,
in a 4:3 viewport it would show sqrt(16*10*4/3)=14.6 by sqrt(16*10*3/4)=10.95 units
-}
fixedArea : Vec2 -> Vec2 -> Camera
fixedArea size position =
    let
        ( w, h ) =
            Vector2.toTuple size
    in
        { area = w * h
        , position = position
        }


-- TODO remove me
scale : Float -> Float2 -> Float2
scale a ( x, y ) =
    ( a * x, a * y )


{-| Calculate the matrix transformation that represents how to transform the
camera back to the origin. The result of this is used in the vertex shader.
-}
view : Vec2 -> Camera -> Mat4
view viewportSize camera =
    let
        -- TODO Use vec2

        viewportSize_ = Vector2.toTuple viewportSize

        ( x, y ) =
            camera.position
                |> snapPosition
                |> Vector2.toTuple

        ( w, h ) =
            scale 0.5 (getViewSize viewportSize_ camera)

        ( l, r, d, u ) =
            ( x - w, x + w, y - h, y + h )
    in
        Matrix4.makeOrtho2D l r d u


{-| Snap camera position to nearest integer value. This is important when passing
camera to camera transformation matrix, since passing unrounded value will cause artifacts
on the final scene.
-}
snapPosition : Vec2 -> Vec2
snapPosition position =
    let
        (x, y) = Vector2.toTuple position
    in
        fromInt (floor x) (floor y)


{-| Get the width and height in game units
-}
getViewSize : ( Float, Float ) -> Camera -> ( Float, Float )
getViewSize ( w, h ) { area } =
    -- TODO convert into Vec2
    ( sqrt (area * w / h), sqrt (area * h / w) )


{-| Move a camera by the given vector *relative* to the camera.
-}
moveBy : Vec2 -> Camera -> Camera
moveBy offset camera =
    { camera | position = Vector2.add camera.position offset }


{-| Move a camera to the given location. In *absolute* coordinates.
-}
moveTo : Vec2 -> Camera -> Camera
moveTo position camera =
    { camera | position = position }


{-| Smoothly follow the given target. Use this in your tick function.

    follow 1.5 dt target camera
-}
follow : Float -> Float -> Vec2 -> Camera -> Camera
follow speed dt targetPosition ({ position } as camera) =
    let
        vectorToTarget =
            Vector2.sub targetPosition position

        newPosition =
            Vector2.add position (Vector2.scale (speed * dt) vectorToTarget)
    in
        { camera | position = newPosition }
