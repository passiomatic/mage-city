module Player exposing
    ( spawn
    , tick
    , render
    , assets
    , objectId
    , collisionFilter
    )

import Keyboard.Extra as Keyboard exposing (Direction(..))
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import Math.Matrix4 exposing (Mat4)

import Resources as Resources exposing (Asset, Resources)
import Render exposing (Uniform(..))
import Object exposing (Object, Category(..), Player)
import Bitwise exposing (or)

atlasAsset : Asset
atlasAsset =
    { name = "Player"
    , url = "images/player.png"
    }


-- Hardcoded id to grab the player object later
objectId =
    0

walkFramesNorth =
    (12, 3, 0.6)


walkFramesEast =
    (8, 3, 0.6)


walkFramesSouth =
    (0, 3, 0.6)


walkFramesWest =
    (4, 3, 0.6)


idleFrames =
    (16, 2, 1.8)


assets =
    [ atlasAsset ]


walkSpeed =
    75


{-| Player Z position in the world -}
zPosition =
    0.35


spriteSize =
    vec2 32 32


collisionSize =
    vec2 26 30


collisionBitMask =
    or Object.collisionObjectCategory
    ( or Object.collisionObstacleCategory Object.collisionTriggerCategory )


{-| Player collide with...  -}
-- collisionMask
collisionFilter : Object -> Bool
collisionFilter object =
    case object.category of
        TriggerCategory ->
            True
        ObstacleCategory ->
            True
        CrateCategory _ ->
            True
        NpcCategory _ ->
            True
        _ ->
            False


spawn : Resources -> Vec2 -> Object
spawn resources position =
    let
        atlas =
            Resources.getTexture atlasAsset.name resources

        category =
            PlayerCategory
                { direction = South
                , atlas = atlas
                , velocity = Vector2.zero
                }
    in
        { category = category
        , id = objectId
        , name = "Player"
        , position = position
        , collisionSize = collisionSize
        , collisionCategory = Object.collisionPlayerCategory
        , collisionBitMask = collisionBitMask
        }


-- MOVEMENT


{-| Called on every update cycle by the game engine
-}
tick : Float -> Keyboard.State -> Object -> Player -> Object
tick dt keys object player =
    let
        direction =
            Keyboard.arrowsDirection keys

        {x, y} =
            Keyboard.arrows keys

        velocty =
            Vector2.scale walkSpeed (Vector2.fromInt x y)

        newPlayer = { player
            | velocity = velocty
            , direction = direction
        }
    in
    { object | category = PlayerCategory newPlayer }


-- RENDERING


render : Float -> Mat4 -> Object -> Player -> Entity
render time cameraProj { position } ({ velocity, atlas, direction } as player) =

    let
        (spriteIndex, frameCount, duration) =
            resolveFrames player

        (x, y) =
            Vector2.toTuple position

        position_ = vec3 x y zPosition

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = Render.makeTransform position_ spriteSize 0 (0.5, 0.5)
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


resolveFrames : Player -> (Int, Int, Float)
resolveFrames player =

    case player.direction of
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
