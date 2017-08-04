module Object exposing
    ( Object
    , Category
    , Category(..)
    , Ai(..)
    , Crate
    , Npc
    , Player
    , move
    , followPath
    , colliding
    --, toRectangle
    , collisionObjectCategory
    , collisionPlayerCategory
    , collisionObstacleCategory
    , collisionTriggerCategory
    )

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Vector2Extra as Vector2
import WebGL.Texture as Texture exposing (Texture)
import Bitwise exposing (and)


{-| The main game object type. See individual file (e.g. Crate.elm)
for actual implementation -}
type Category
    = TriggerCategory
    | ObstacleCategory
    | CrateCategory Crate
    | NpcCategory Npc
    | PlayerCategory Player


type Ai
    = Spawned
    | Stand
    | FollowPath (List Vec2)


{-| A generic enough category, used as a sort of
catch-all value for enemies, NPC and other items
-}
collisionObjectCategory =
    1

{-| Collision category for player
-}
collisionPlayerCategory =
    2

{-| Collision category for obstacles
-}
collisionObstacleCategory =
    4

{-| Collision category for triggers
-}
collisionTriggerCategory =
    8


type alias Crate = TexturedObject
    { isOpen : Bool
    }


type alias Npc = TexturedObject
    { velocity : Vec2
    , ai : Ai
    }


type alias Player = TexturedObject
    { direction : Direction
    , velocity : Vec2
    }


{-| A generic object in the level
-}
type alias Object =
    { category : Category
    , id : Int
    , name : String
    , position : Vec2
    , collisionSize : Vec2
    , collisionCategory : Int
    , collisionBitMask : Int
    }


{-| Object with an associated texture atlas with
potentially multiple appearance -}
type alias TexturedObject a =
    { a | atlas: Texture }


{- Object with a simple looping animation -}
-- type alias AnimatedObject a =
--     { a | atlas: Texture, frameCount: Int, duration : Float }


{-| Convenience type alias used during collision detection
-}
type alias Rectangle =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


-- MOVEMENT


{- Move object with the given velocity vector
-}
move : Float -> Vec2 -> Object -> Object
move dt velocity ({ position } as object)  =
    { object
        | position = Vector2.add position (Vector2.scale dt velocity)
    }


{- Calculate object velocity vector using the given path
-}
followPath : Float -> List Vec2 -> Vec2 -> (List Vec2, Vec2)
followPath speed points position =
    let
        (newPoints, targetPosition) =
            nextPoint points position

        targetVector =
            Vector2.direction targetPosition position
    in
        (newPoints, Vector2.scale speed targetVector)


nextPoint : List Vec2 -> Vec2 -> (List Vec2, Vec2)
nextPoint points position =
    case points of
        firstPoint :: secondPoint :: rest ->
            if Vector2.distance firstPoint position < 0.5 then
                let
                    newPoints =
                        secondPoint :: (rest ++ [ firstPoint ])
                in
                    (newPoints, secondPoint)
            else
                -- Not there yet, keep using the list head
                (points, firstPoint)

        -- Handle "degenerate" cases

        firstPoint :: [] ->
            -- Just reach the point
            (points, firstPoint)

        [] ->
            -- Nowhere to go, stand still
            (points, position)


-- COLLISION


{- Return the game objects colliding with target object
-}
colliding : List Object -> Object -> List Object
colliding objects targetObject =
    let
        targetRect = toRectangle targetObject
    in
        objects
            |> List.filter (canCollide targetObject.collisionBitMask)
            |> List.filter (isColliding targetRect)


canCollide : Int -> Object -> Bool
canCollide collisionBitMask object =
    and collisionBitMask object.collisionCategory /= 0


isColliding : Rectangle -> Object -> Bool
isColliding rect1 object =
    let
        rect2 = toRectangle object
    in
        if rect1.x < rect2.x + rect2.w &&
           rect1.x + rect1.w > rect2.x &&
           rect1.y < rect2.y + rect2.h &&
           rect1.h + rect1.y > rect2.y then
               True
        else
               False

toRectangle : Object -> Rectangle
toRectangle object =
    let
        (cx, cy) =
            Vector2.toTuple object.position

        (w, h) =
            Vector2.toTuple object.collisionSize

        startingPoint centerPoint length =
            centerPoint - (length / 2)

        x =
            startingPoint cx w
        y =
            startingPoint cy h
    in
        { x = x, y = y, w = w, h = h }
