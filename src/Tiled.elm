module Tiled exposing
    ( Level
    , Layer
    , Placeholder
    , Geometry(..)
    , tileSet
    , tileSize
    )

{-| Level data structures. Only a small subset of Tiled editor features
is supported by current implementation. For additional details see:
http://doc.mapeditor.org/reference/tmx-map-format/
-}
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Color exposing (Color)
import Resources exposing (Asset)


tileSize =
    vec2 32 32


{-| All the levels share the same tile set -}
tileSet =
    { name = "City"
    , url = "images/mage-city-tileset.png"
    }


{-| Game level
-}
type alias Level =
    { name : String
    , background : Color
    , assets : List Asset
    , layers : List Layer
    , placeholders : List Placeholder
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
    | PolygonGeometry  { points : List Vec2 }


{-| Tile layer
-}
type alias Layer =
    { name : String
    -- TODO opacity : Float
    , visible : Bool
    , position : Vec2
    , size : Vec2
    , lutName : String
    }
