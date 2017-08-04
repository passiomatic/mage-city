module Player exposing
    ( spawn
    , update
    , render
    , objectId
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
import Assets
import Model exposing (Model)


objectId =
    0 -- Hardcoded id to grab the player object later

walkFramesNorth =
    (12, 3, 0.6)


walkFramesEast =
    (8, 3, 0.6)


walkFramesSouth =
    (0, 3, 0.6)


walkFramesWest =
    (4, 3, 0.6)


idleFrames =
    (16, 1, 1)


shadowSpriteIndex =
    3


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
   Object.collisionTriggerCategory
     |> or Object.collisionObstacleCategory
     |> or Object.collisionObjectCategory


spawn : Resources -> Vec2 -> Object
spawn resources position =
    let
        playerAsset =
            Assets.player

        atlas =
            Resources.getTexture playerAsset.name resources

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
update : Model -> Object -> Player -> Object
update model object player =
    let
        direction =
            Keyboard.arrowsDirection model.keys

        { x, y } =
            Keyboard.arrows model.keys

        velocty =
            Vector2.scale walkSpeed (Vector2.fromInt x y)

        newPlayer = { player
            | velocity = velocty
            , direction = direction
        }
    in
    { object | category = PlayerCategory newPlayer }


-- RENDERING

render : Float -> Mat4 -> Object -> Player -> List Entity
render time cameraProj { position } ({ atlas } as player) =

    let
        (x, y) =
            Vector2.toTuple position

        playerPosition =
            vec3 x y zPosition

        shadowPosition =
            vec3 x (y - 4) (zPosition - 0.01)

    in
        [ renderPlayer time cameraProj playerPosition player
        , Render.renderSprite cameraProj shadowPosition atlas spriteSize shadowSpriteIndex
        ]


renderPlayer : Float -> Mat4 -> Vec3 -> Player -> Entity
renderPlayer time cameraProj position ({ atlas } as player) =

    let
        (spriteIndex, frameCount, duration) =
            resolveFrames player

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = Render.makeTransform position spriteSize 0 (0.5, 0.5)
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
