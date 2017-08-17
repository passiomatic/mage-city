module Objects.Npc
    exposing
        ( spawn
        , update
        , render
        )

{-| NPC (Non-Player Character) game object implementation
-}

import Keyboard.Extra as Keyboard exposing (Direction(..))
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Render exposing (Uniform(..))
import Objects.Object as Object exposing (Object, Category(..), Ai(..), Npc)
import Assets exposing (Assets)
import Bitwise exposing (or)
import Model exposing (Model)
import Dict exposing (Dict)
import Tiled exposing (Placeholder, Geometry(..))


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


walkSpeed =
    60


zPosition =
    0.33 -- Just behind the player


spriteSize =
    vec2 32 32


collisionSize =
    vec2 26 30


collisionBitMask =
    Object.collisionObstacleCategory
        |> or Object.collisionPlayerCategory


spawn : Assets -> Int -> String -> Vec2 -> Object
spawn assets id name position =
    let
        npcAsset =
            Assets.npc

        atlas =
            Assets.texture npcAsset.name assets

        category =
            NpcCategory
                { atlas = atlas
                , velocity = Vector2.zero
                , ai = Spawned
                }
    in
        { category = category
        , id = id
        , name = name
        , position = position
        , collisionSize = collisionSize
        , collisionCategory = Object.collisionObjectCategory
        , collisionBitMask = collisionBitMask
        }



-- MOVEMENT


{-| Called on every update cycle by the game engine
-}
update : Model -> Object -> Npc -> Object
update ( { level } as model ) ( { position, name } as object ) npc =

    let
        newNpc =
            case npc.ai of
                Spawned ->
                    { npc | ai = resolveAi level.placeholders name }

                Stand ->
                    { npc | velocity = Vector2.zero }

                FollowPath points ->
                    let
                        (newPoints, newVelocity) =
                            Object.followPath walkSpeed points position
                    in
                        { npc
                        | ai = FollowPath newPoints
                        , velocity = newVelocity
                        }
    in
        { object | category = NpcCategory newNpc }


forestPath1 =
    57

{-| Different NPC's have different AI
-}
resolveAi : Dict Int Placeholder -> String -> Ai
resolveAi placeholders name =
    case name of
        "Citizen1" ->
            Dict.get forestPath1 placeholders
                |> unwrapAi

        _ ->
            Stand

unwrapAi : Maybe Placeholder -> Ai
unwrapAi placeholder =
    Maybe.withDefault Stand ( Maybe.map (\placeholder ->
        case placeholder.geometry of
            PolygonGeometry path ->
                FollowPath path.points

            _ ->
                Stand
        ) placeholder )


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

        South ->
            walkFramesSouth

        West ->
            walkFramesWest

        _ ->
            idleFrames
