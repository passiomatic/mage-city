module Tiled exposing
    ( Level
    , Layer
    , Placeholder
    , Geometry(..)
    , tileSize
    )

{-| Level data structures. Only a small subset of Tiled editor features
is supported by current implementation. For additional details see:
http://doc.mapeditor.org/reference/tmx-map-format/
-}
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Color exposing (Color)
import Dict exposing (Dict)
import Assets exposing (AssetDescription)


tileSize =
    vec2 32 32


{-| Game level
-}
type alias Level =
    { name : String
    , background : Color
    , assets : List AssetDescription
    , layers : List Layer
    , placeholders : Dict Int Placeholder
    }


{-| Object placeholder in the level file. It will be turned
into a game object after a spawn operation (see Scene.elm)
-}
type alias Placeholder =
    { categoryName : String
    , id : Int
    , name : String
    , geometry : Geometry
    }


type Geometry
    = RectangleGeometry { position : Vec2, size : Vec2 }
    | PolygonGeometry { points : List Vec2 }


{-| Tile layer
-}
type alias Layer =
    { name : String
    -- TODO opacity : Float
    , isVisible : Bool
    , position : Vec3
    , size : Vec2
    }
