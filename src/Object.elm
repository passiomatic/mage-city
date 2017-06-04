module Object exposing
    ( Object
    , Category
    , Category(..)
    , Crate
    )

import Math.Vector2 as Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import WebGL.Texture as Texture exposing (Texture)


{-| The main game object type. See individual file (e.g. Crate.elm)
for actual implementation -}
type Category
    = TriggerCategory
    | ObstacleCategory
    | CrateCategory Crate


type alias Crate = TexturedObject { isOpen : Bool }


{-| A generic object in the level
-}
type alias Object =
    { category : Category
    , id : Int
    , name : String
    , position : Vec2
    , collisionSize: Vec2
    }


{-| Object with an associated texture atlas with possibly multiple appearance -}
type alias TexturedObject a =
    { a | atlas: Texture }


{-| Object with an associated animation -}
type alias AnimatedObject a =
    { a | atlas: Texture, frameCount: Int, duration : Float }
