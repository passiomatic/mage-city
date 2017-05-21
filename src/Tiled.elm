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
import Vector2Extra as Vector2
import Vector3Extra as Vector3

import Color exposing (Color)
import Dict exposing (Dict)
import Resources as Resources exposing (Resources, Asset)
import Render exposing (makeTransform, toEntity, Uniform(..))

import Collision


-- A tile is 32x32 pixels
tileSize =
    vec2 32 32


-- The main game tileset
tileSet : TileSet
tileSet =
    { name = "City"
    , url = "images/mage-city-tileset.png"
    }


type alias Level =
    { name : String
    , background : Color
    , assets : List Asset
    , layers : List Layer
    , obstacles : List Obstacle
    , spawns : List Spawn
    }


type alias TileSet =
    { url : String
    , name : String
    }


type alias Obstacle =
    { name : String
    , id : Int
    , rect : Collision.Rectangle
    }

-- A entity placeholder in the level
type alias Spawn =
    { type_ : String
    , name : String
    , id : Int
    , position : Vec2
    , size : Vec2
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
        position = Vector3.fromVec2 layer.position zPosition

        atlas =
            Resources.getTexture tileSet.name resources

        lut =
            Resources.getTexture layer.lutName resources

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = makeTransform position layer.size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , lut = lut
            , atlasSize = Vector2.fromInt atlasW atlasH
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

        position_ =
            Vector3.fromVec2 position zPosition

        uniforms =
            { transform = makeTransform position_ size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , color = Vector3.fromColor Color.blue
            }
    in
        toEntity (ColoredRect uniforms)
