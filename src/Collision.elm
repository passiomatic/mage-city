module Collision
    exposing
        ( axisAlignedBoundingBox
        , Side(Top, Right, Bottom, Left)
        , rectangleSide
        , Rectangle
        , rectangle
        )

{-| Detect collision/intersection of geometry in a defined 2D coordinate space
AKA tell me when objects are touching or overlapping.

Code adapted from https://github.com/burabure/elm-collision

# Basic geometry
@docs Rectangle, rectangle

# Rectangle to Rectangle Collision
@docs axisAlignedBoundingBox, rectangleSide, Side

-}

import Math.Vector2 as Vector2 exposing (Vec2, vec2)

{-| Represents rectangular hitbox geometry
-}
type Rectangle
    = Rectangle { cx : Float, cy : Float, w : Float, h : Float }


{-| Create a Rectangle hitbox from coordinates (cx, cy) and geometry (width and height)
-}
rectangle : Vec2 -> Vec2 -> Rectangle
rectangle position size =
    let
        (cx, cy) =
            Vector2.toTuple position

        (w, h) =
            Vector2.toTuple size
    in
        Rectangle { cx = cx, cy = cy, w = w, h = h }


{-| Super efficient collision detection between
two Rectangles that are axis aligned — meaning no rotation.

    rect1 = rectangle (vec2 5 5) (vec2 10 10)
    rect2 = rectangle (vec2 7 5) (vec2 10 10)

    axisAlignedBoundingBox rect1 rect2 -- True
    -- rect1 is colliding with rect2
-}
axisAlignedBoundingBox : Rectangle -> Rectangle -> Bool
axisAlignedBoundingBox (Rectangle rect1) (Rectangle rect2) =
  let
    startingPoint centerPoint length =
        centerPoint - (length / 2)

    x1 = startingPoint rect1.cx rect1.w
    x2 = startingPoint rect2.cx rect2.w
    y1 = startingPoint rect1.cy rect1.h
    y2 = startingPoint rect2.cy rect2.h
  in
    if x1 < x2 + rect2.w &&
       x1 + rect1.w > x2 &&
       y1 < y2 + rect2.h &&
       rect1.h + y1 > y2 then
      True
    else
      False


{-| Represents sides of a Rectangle
-}
type Side
    = Top
    | Right
    | Bottom
    | Left


{-| Very efficiently detect which side of a Rectangle is colliding with another Rectangle

    rect1 = rectangle (vec2 5 5) (vec2 10 10)
    rect2 = rectangle (vec2 7 5) (vec2 10 10)

    rectangleSide rect1 rect2 -- Just Right
    -- rect1 is coliding with it's right side onto rect2
-}
rectangleSide : Rectangle -> Rectangle -> Maybe Side
rectangleSide (Rectangle rect1) (Rectangle rect2) =
  {-
    Calculate which side of a rectangle is colliding w/ another, it works by
    getting the Minkowski sum of rect2 and rect1, then checking where the centre of
    rect1 lies relatively to the new rectangle (from Minkowski) and to its diagonals.
    Thanks to Sam Hocevar @samhocevar for the formula!
  -}
  let
    w = 0.5 * (rect1.w + rect2.w)
    h = 0.5 * (rect1.h + rect2.h)
    dx = rect2.cx - rect1.cx
    dy = rect2.cy - rect1.cy
    wy = w * dy
    hx = h * dx
  in
    if abs dx <= w && abs dy <= h then
      if (wy > hx) then
        if (wy > -hx) then
          Just Top
        else
          Just Left
      else
        if (wy > -hx) then
          Just Right
        else
          Just Bottom
    else
      Nothing
