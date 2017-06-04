module Player exposing
    ( Player
    , initialModel
    , tick
    , render
    , collision
    , assets
    )

import Color
import Keyboard.Extra as Keyboard exposing (Direction, Direction(..))
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import Math.Matrix4 exposing (Mat4)

import Resources as Resources exposing (Resources)
import Render exposing (makeTransform, toEntity, Uniform(..))
import Collision exposing (Side(..))
import Object exposing (Object)


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

{-| Player Z position in the world -}
zPosition =
    0.35

{-| Smaller than sprite size -}
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


{-| Calculate next player position with s = v * dt
-}
nextPosition : Float -> Player -> Player
nextPosition dt player =
    { player
        | position = Vector2.add player.position (Vector2.scale dt player.velocity)
    }


{-| Figure put the colliding objects with player
-}
collision : List Object -> Player -> List Object
collision objects player =
    let
        playerRect = Collision.rectangle player.position collisionSize

        --isColliding : Object -> Bool
        isColliding { position, collisionSize }  =
            let
                rect = Collision.rectangle position collisionSize
            in
                Collision.axisAlignedBoundingBox rect playerRect
    in
        List.filter isColliding objects


{-| Called on every update cycle by the AnimationFrame subscription
-}
tick : Float -> Keyboard.State -> Player -> Player
tick dt keys player  =
    -- Figure out next player position
    player
        |> walk keys
        |> nextPosition dt


{-| Query keyboard keys to figure out walk velocity vector
-}
walk : Keyboard.State -> Player -> Player
walk keys player =
    let
        direction =
            Keyboard.arrowsDirection keys

        {x, y} =
            Keyboard.arrows keys
    in
        { player
            | velocity = Vector2.scale walkSpeed (Vector2.fromInt x y)
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

        position_ =
            Vector3.fromVec2 position zPosition

        atlas =
            Resources.getTexture (Tuple.first atlasAsset) resources

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = makeTransform position_ size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , frameCount = frameCount
            , spriteIndex = spriteIndex
            , duration = duration
            , time = time
            , spriteSize = size
            , atlasSize = Vector2.fromInt atlasW atlasH
            }
    in
        toEntity (AnimatedRect uniforms)
