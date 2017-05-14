module Tiled exposing (Level, renderLevel, renderObstacles, tileSet, tileSize, Obstacle)

{-| # Tiled editor support
This module renders game levels produced by Tiled Editor software and
processed by the Elmify utility. Only a small subset of editor features
is supported by current implementation.

For additional details see: http://doc.mapeditor.org/reference/tmx-map-format/
-}
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)

import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2 exposing (fromInt)

import Color exposing (Color)
import Dict exposing (Dict)
import Resources as Resources exposing (Resources, Asset)
import Render exposing (makeTransform, toEntity, Uniform(..))

import Collision
import Helpers exposing (midpoint, toVec3, colorToVec3)


-- A tile is 32x32 pixels
tileSize =
    vec2 32 32


-- The main game tileset
tileSet : TileSet
tileSet =
    { name = "City"
    , url = "images/mage-city-tileset.png"
    , size = vec2 256 2048
    }


type alias Level =
    { name : String
    , background : Color
    , assets : List Asset
    , layers : List Layer
    , obstacles: List Obstacle
        }


type alias TileSet =
    { url : String
    , name : String
    , size : Vec2
    }

-- TODO "draworder":"topdown"
-- type alias Object =
--     { name : String
--     --, visible : Bool
--     , width : Float
--     , height : Float
--     , x : Float
--     , y : Float
--     }


type alias Obstacle =
    { name : String
    , id : Int
    , rect : Collision.Rectangle
    }


-- Level tiles
type alias Layer =
    { name : String
    -- TODO opacity : Float
    , visible : Bool
    , position : Vec2
    , size : Vec2
    , lutName : String
    }


-- RENDERING


renderLevel : Resources -> Mat4 -> Level -> List Entity
renderLevel resources cameraProj level =
    level.layers
        |> List.filter (\layer -> layer.visible)
        |> List.indexedMap (\index layer ->
            -- TODO Assign z value from Tiled editor
            renderLayer resources cameraProj layer (toFloat index / 10)
        )


renderLayer : Resources -> Mat4 -> Layer -> Float -> Entity
renderLayer resources cameraProj layer zPosition =
    let
        position = toVec3 layer.position zPosition

        atlas =
            Resources.getTexture tileSet.name resources

        lut =
            Resources.getTexture layer.lutName resources

        (atlasW, atlasH) = Texture.size atlas

        uniforms =
            { transform = makeTransform position layer.size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , lut = lut
            , atlasSize = fromInt atlasW atlasH
            , layerSize = layer.size
            , tileSize = tileSize
            }
    in
        toEntity (TiledRect uniforms)



{- Obstacles are usually invisible, but let's render them
on request to ease debugging
-}
renderObstacles : Resources -> Mat4 -> Level -> (List Entity)
renderObstacles resources cameraProj level =
    List.map (\obstacle ->
            renderObstacle cameraProj obstacle.rect 0
        ) level.obstacles


renderObstacle: Mat4 -> Collision.Rectangle -> Float -> Entity
renderObstacle cameraProj rect zPosition =
    let
        (position, size) =
            Collision.toTuple rect

        position_ = toVec3 position zPosition

        uniforms =
            { transform = makeTransform position_ size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , color = colorToVec3 Color.blue
            }
    in
        toEntity (ColoredRect uniforms)
