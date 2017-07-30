module Npc
    exposing
        ( spawn
        , tick
        , render
        )

{-| NPC (Non-Player Character) game object implementation.
-}

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Resources as Resources exposing (Asset, Resources)
import Render exposing (Uniform(..))
import Object exposing (Object, Category(..), Npc)
import Assets


walkFramesNorth =
    ( 12, 3, 0.6 )


walkFramesEast =
    ( 8, 3, 0.6 )


walkFramesSouth =
    ( 0, 3, 0.6 )


walkFramesWest =
    ( 4, 3, 0.6 )


idleFrames =
    ( 16, 2, 2.8 )


shadowSpriteIndex =
    3

zPosition =
    0.33 -- Just behind the player


spriteSize =
    vec2 32 32


collisionSize =
    vec2 26 30


spawn : Resources -> Int -> String -> Vec2 -> Object
spawn resources id name position =
    let
        npcAsset =
            Assets.npc

        atlas =
            Resources.getTexture npcAsset.name resources

        category =
            NpcCategory
                { atlas = atlas
                , velocity = Vector2.zero
                , targetPosition = position
                }
    in
        { category = category
        , id = id
        , name = name
        , position = position
        , collisionSize = collisionSize
        , collisionCategory = Object.collisionObjectCategory
        , collisionBitMask = 0 -- FIXME
        }



-- MOVEMENT


{-| Called on every update cycle by the game engine
-}
tick : Float -> Object -> Npc -> Object
tick dt object npc =
    -- Figure out next NPC position
    { object
        | position = Vector2.add object.position (Vector2.scale dt npc.velocity)
    }



-- RENDERING

render : Float -> Mat4 -> Object -> Npc -> List Entity
render time cameraProj { position } ({ atlas } as npc) =

    let
        (x, y) =
            Vector2.toTuple position

        npcPosition =
            vec3 x y zPosition

        shadowPosition =
            vec3 x (y - 4) (zPosition - 0.01)

    in
        [ renderNpc time cameraProj npcPosition npc
        , Render.renderSprite cameraProj shadowPosition atlas spriteSize shadowSpriteIndex
        ]


renderNpc : Float -> Mat4 -> Vec3 -> Npc -> Entity
renderNpc time cameraProj position ({ atlas } as npc) =
    let
        (spriteIndex, frameCount, duration) =
            resolveFrames npc

        ( atlasW, atlasH ) =
            Texture.size atlas

        uniforms =
            { transform = Render.makeTransform position spriteSize 0 ( 0.5, 0.5 )
            , cameraProj = cameraProj
            , atlas = atlas
            , frameCount = frameCount
            , spriteIndex = spriteIndex
            , duration = duration
            , time = time
            , spriteSize = spriteSize
            , atlasSize = Vector2.fromInt atlasW atlasH
            }
    in
        Render.toEntity (AnimatedRect uniforms)


resolveFrames : Npc -> (Int, Int, Float)
resolveFrames npc =

    case Vector2.toDirection npc.velocity of
        North ->
            walkFramesNorth

        East ->
            walkFramesEast

        NorthEast ->
            walkFramesEast

        SouthEast ->
            walkFramesEast

        South ->
            walkFramesSouth

        West ->
            walkFramesWest

        NorthWest ->
            walkFramesWest

        SouthWest ->
            walkFramesWest

        _ ->
            idleFrames
