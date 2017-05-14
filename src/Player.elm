module Player exposing (Player, initialModel, tick, render, assets)

import Color
import Keyboard.Extra as Keyboard exposing (Direction, Direction(..))

import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)

import Math.Vector2 as Vector2 exposing
    (Vec2, vec2, getX, getY, add, scale)
import Vector2Extra as Vector2 exposing (fromInt)

import Math.Matrix4 exposing (Mat4)

import Tiled exposing (Level, Obstacle)
import Resources as Resources exposing (Resources)
import Render exposing (makeTransform, toEntity, Uniform(..))

import Collision exposing (Side(..))

import Helpers as Helpers exposing (..)

-- TODO
-- https://groups.google.com/d/msg/elm-discuss/AaL8iLjhEdU/e4A608rtAQAJ
--type alias Positioned = { position: Vec2 }
-- type GameEntity =
--    Player Positioned
--  | Enemy Positioned
--  | Item Positioned Textured ????

atlasAsset =
    ("Player", "images/player.png")

-- TODO
-- type Frames
--     = WalkNorth 12 3
--     | WalkEast 8 3
--     | WalkSouth 0 3
--     | WalkWest 4 3


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

-- Player Z position in the world
zPosition =
    0.45

-- Smaller than sprite size
collisionSize =
    vec2 26 30


type alias Player =
    { size : Vec2  -- TODO Should this stay into the model?
    , direction : Direction
    , position : Vec2
    , velocity : Vec2
    }


initialModel : Player
initialModel =
    { size = vec2 32 32
    , direction = South
    , position = vec2 200 100
    , velocity = vec2 0 0
    }

-- MOVEMENT

-- Called on every update cycle by the AnimationFrame subscription
tick : Float -> Keyboard.State -> List Obstacle -> Player -> Player
tick dt keys obstacles player  =
    player
        |> walk keys
        |> collision obstacles
        |> physics dt


-- Figure out next player position with s = v * dt
physics : Float -> Player -> Player
physics dt player =
    { player
        | position = add player.position (scale dt player.velocity)
    }


-- Check collisions against level obstacles
collision : List Obstacle -> Player -> Player
collision obstacles player =

    -- Avoid collision checking is player is not moving
    if (Vector2.length player.velocity) == 0 then
        player
    else
        let
            playerRect = Collision.rectangle player.position collisionSize

            -- isColliding rect =
            --     Collision.axisAlignedBoundingBox rect playerRect

            newVelocity = List.foldl (\obstacle velocity ->
                    --if isColliding obstacle.rect then
                    collisionVelocity (Collision.rectangleSide playerRect obstacle.rect) player.direction velocity
                    --else
                    -- velocity
                ) player.velocity obstacles
        in
            { player | velocity = newVelocity }


-- Set velocity to zero only if the player direction is towards the obstacle
collisionVelocity : Maybe Side -> Direction -> Vec2 -> Vec2
collisionVelocity side direction velocity =

    case (side, direction) of
        (Just Top, North) ->
            Vector2.zero

        (Just Right, East) ->
            Vector2.zero

        (Just Bottom, South) ->
            Vector2.zero

        (Just Left, West) ->
            Vector2.zero

        (_, _) ->
            velocity


walk : Keyboard.State -> Player -> Player
walk keys player =
    let
        direction =
            Keyboard.arrowsDirection keys

        {x, y} =
            Keyboard.arrows keys
    in
        { player
            | velocity = scale walkSpeed (fromInt x y)
            , direction = direction
        }


-- RENDERING

render : Resources -> Float -> Mat4 -> Player -> Entity
render resources time cameraProj ({ size, direction, position } as player) =
    let
        (spriteIndex, frameCount, duration) =
            case direction of
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

        position_ = toVec3 position zPosition

        atlas = Resources.getTexture (Tuple.first atlasAsset) resources

        (atlasW, atlasH) = Texture.size atlas

        uniforms =
            { transform = makeTransform position_ size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , frameCount = frameCount
            , spriteIndex = spriteIndex
            , duration = duration
            , time = time
            , spriteSize = size
            , atlasSize = fromInt atlasW atlasH
            }
    in
        toEntity (AnimatedRect uniforms)
