module Object exposing
    ( Object
    , Category
    , Category(..)
    , Crate
    , Npc
    , Player
    , move
    , canCollide
    , isColliding
    , toRectangle
    , collisionObjectCategory
    , collisionPlayerCategory
    , collisionObstacleCategory
    , collisionTriggerCategory
    )

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import WebGL.Texture as Texture exposing (Texture)
import Collision exposing (Rectangle)
import Bitwise exposing (and)


{-| The main game object type. See individual file (e.g. Crate.elm)
for actual implementation -}
type Category
    = TriggerCategory
    | ObstacleCategory
    | CrateCategory Crate
    | NpcCategory Npc
    | PlayerCategory Player


{-| A generic enough category, used as a sort of
catch-all value for enemies, NPC and other items
-}
collisionObjectCategory =
    1


collisionPlayerCategory =
    2


collisionObstacleCategory =
    4


collisionTriggerCategory =
    8


type alias Crate = TexturedObject
    { isOpen : Bool
    }


type alias Npc = TexturedObject
    { velocity : Vec2
    , targetPosition : Vec2
    -- direction : Direction
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


{- Calculate next object position with s = v * dt
-}
move : Float -> Vec2 -> Object -> Object
move dt velocity ({ position } as object)  =
    { object
        | position = Vector2.add position (Vector2.scale dt velocity)
    }


{- Check object collision mask again other object collision category
-}
canCollide : Object -> Object -> Bool
canCollide object otherObject =
    and object.collisionBitMask otherObject.collisionCategory /= 0


{- Check if object is colliding with target rectangle
-}
isColliding : Rectangle -> Object -> Bool
isColliding rect { position, collisionSize } =
    let
        objectRect = Collision.rectangle position collisionSize
    in
        Collision.axisAlignedBoundingBox objectRect rect


toRectangle object =
    Collision.rectangle object.position object.collisionSize
