module Npc
    exposing
        ( spawn
        , tick
        , render
        , assets
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


atlasAsset : Asset
atlasAsset =
    { name = "Npc"
    , url = "images/npc-1.png"
    }


assets =
    [ atlasAsset
    ]


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


zPosition =
    0.33 -- Just behind the player


spriteSize =
    vec2 32 32



-- Smaller than sprite size
collisionSize =
    vec2 26 30


spawn : Resources -> Int -> String -> Vec2 -> Object
spawn resources id name position =
    let
        atlas =
            Resources.getTexture atlasAsset.name resources

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


render : Float -> Mat4 -> Object -> Npc -> Entity
render time cameraProj { position } ({ velocity, atlas } as npc) =
    let
        (spriteIndex, frameCount, duration) =
            resolveFrames npc

        ( x, y ) =
            Vector2.toTuple position

        position_ =
            vec3 x y zPosition

        ( atlasW, atlasH ) =
            Texture.size atlas

        uniforms =
            { transform = Render.makeTransform position_ spriteSize 0 ( 0.5, 0.5 )
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
