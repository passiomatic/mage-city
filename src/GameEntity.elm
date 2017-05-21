module GameEntity exposing (..)

import Color exposing (Color)
import Resources as Resources exposing (Resources, Asset)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3

import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)

import Render exposing (makeTransform, toEntity, Uniform(..))


crateAsset =
    ("Crate", "images/crate.png")

crateSize =
    vec2 32 32

assets =
    [ crateAsset
    ]

-- Just behind the player
zPosition =
    0.33

type GameEntity
    = Trigger (WithPosition {})
    | Crate (WithPosition (WithTexture { isOpen: Bool }))


type alias WithPosition a =
    { a | position: Vec2, size: Vec2 }


type alias WithTexture a =
    { a | atlas: Texture }


-- type alias WithAnimation a =
--     { a | texture: Texture, frameCount: Int }

spawn : Resources -> String -> Vec2 -> Vec2 -> GameEntity
spawn resources type_ position size =
    case type_ of
        "Trigger" ->
            Trigger
                { position = position
                , size = size
                }

        "Crate" ->
            let
                atlas = Resources.getTexture "Crate" resources
            in
                Crate
                    { position = position
                    , size = crateSize
                    , atlas = atlas
                    , isOpen = False
                    }

        _ ->
            Debug.crash ("Could not spawn game entity " ++ type_)


-- RENDERING

render : Resources -> Float -> Mat4 -> GameEntity -> Entity
render resources time cameraProj gameEntity =

    let
        -- uniforms a =
        --     { a | transform = makeTransform position_ size 0 (0.5, 0.5)
        --     ,
        --     }

        rendender =
            case gameEntity of
                Trigger  { position, size } ->
                    let
                        position_ = vec3 (Vector2.getX position) (Vector2.getY position) zPosition

                        uniforms =
                            { transform = makeTransform position_ size 0 (0.5, 0.5)
                            , cameraProj = cameraProj
                            , color = Vector3.fromColor Color.red
                            }
                    in
                        ColoredRect uniforms

                Crate { position, atlas, isOpen } ->
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
                        TexturedRect uniforms
    in
        toEntity rendender
