module Crate exposing
    ( spawn
    , render
    , assets
    )

import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3

import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Resources as Resources exposing (Asset, Resources)
import Tiled exposing (Placeholder)
import Render exposing (makeTransform, toEntity, Uniform(..))
import Object exposing (Object, Category(..), Crate)


atlasAsset : Asset
atlasAsset =
    { name = "Crate"
    , url = "images/crate.png"
    }


crateSize =
    vec2 32 32


assets =
    [ atlasAsset
    ]


-- Just behind the player
zPosition =
    0.33


-- Smaller than sprite size
collisionSize =
    vec2 26 30


spawn : Resources -> Placeholder -> Object
spawn resources { id, name, position } =
    let
        atlas = Resources.getTexture atlasAsset.name resources
    in
        { category = CrateCategory { atlas = atlas, isOpen = False }
        , id = id
        , name = name
        , position = position
        , collisionSize = collisionSize
        }


-- RENDERING


render : Float -> Mat4 -> Object -> Crate -> Entity
render time cameraProj { position } { atlas, isOpen } =

    let
        position_ = vec3 (Vector2.getX position) (Vector2.getY position) zPosition

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = makeTransform position_ crateSize 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , atlasSize = Vector2.fromInt atlasW atlasH
            , spriteSize = crateSize
            , spriteIndex = 0 -- TODO isOpen
            }
    in
        toEntity ( TexturedRect uniforms )
