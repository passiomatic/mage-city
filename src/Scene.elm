module Scene
    exposing
        ( spawnObjects
        , updateObjects
        , renderObjects
        , renderLevel
        , resolveCollisions
        )

{-| Scene creation, updating and rendering functions
-}

import Resources as Resources exposing (Resources, Asset)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Tiled exposing (Level, Layer, Placeholder, Geometry(..), tileSize, tileSet)
import Render exposing (Uniform(..))
import Object exposing (Object, Player, Category(..))
import Player
import Crate
import Npc
import Dict exposing (Dict)
import Collision exposing (Rectangle)
import Keyboard.Extra as Keyboard exposing (Direction(..))


-- CREATION


spawnObjects : Resources -> List Placeholder -> List Object
spawnObjects resources placeholders =
    let
        spawner : Placeholder -> (List Object -> List Object)
        spawner placeholder =
            case spawnObject resources placeholder of
                Just object ->
                    (::) object

                Nothing ->
                    identity
    in
        List.foldl spawner [] placeholders


spawnObject : Resources -> Placeholder -> Maybe Object
spawnObject resources ({ categoryName, id, name, geometry } as placeholder) =
    case ( categoryName, geometry ) of
        ( "TriggerCategory", RectangleGeometry { position, size } ) ->
            Just
                { category = TriggerCategory
                , id = id
                , name = name
                , position = position
                , collisionSize = size
                , collisionCategory = Object.collisionTriggerCategory
                , collisionBitMask = 0
                }

        ( "ObstacleCategory", RectangleGeometry { position, size } ) ->
            Just
                { category = ObstacleCategory
                , id = id
                , name = name
                , position = position
                , collisionSize = size
                , collisionCategory = Object.collisionObstacleCategory
                , collisionBitMask = 0
                }

        ( "CrateCategory", RectangleGeometry { position } ) ->
            Just (Crate.spawn resources id name position)

        ( "NpcCategory", RectangleGeometry { position } ) ->
            Just (Npc.spawn resources id name position)

        ( "PlayerCategory", RectangleGeometry { position } ) ->
            Just (Player.spawn resources position)

        _ ->
            Debug.log ("Could not spawn object " ++ categoryName) Nothing



-- UPDATING


{-| Update all the scene objects
-}
updateObjects : Float -> Keyboard.State -> Dict Int Object -> Dict Int Object
updateObjects dt keys objects =
    let
        updater : Int -> Object -> Object
        updater id object =
            case object.category of
                PlayerCategory player ->
                    Player.tick dt keys object player

                NpcCategory npc ->
                    Npc.tick dt object npc

                _ ->
                    -- Just return the original object
                    object
    in
        Dict.map updater objects


{-| Check every object against the others, avoiding dumb combinations
and update each object state accordingly
-}
resolveCollisions : Float -> Dict Int Object -> Dict Int Object
resolveCollisions dt objects =
    let
        resolver : Int -> Object -> Object
        resolver id object =
            case object.category of
                PlayerCategory player ->
                    resolvePlayerCollisions dt object player objects

                -- TODO
                -- NpcCategory npc ->

                _ ->
                    -- Just return the original object
                    object
    in
        Dict.map resolver objects



resolvePlayerCollisions : Float -> Object -> Player -> Dict Int Object -> Object
resolvePlayerCollisions dt playerObject player objects =

    let
        newPlayerObject =
            Object.move dt player.velocity playerObject

        collidingObjects =
            Dict.values objects
                |> List.filter (Object.canCollide playerObject)
                |> List.filter (Object.isColliding (Object.toRectangle newPlayerObject))

        updater : Object -> Object -> Object
        updater otherObject playerObject =
            case otherObject.category of
                ObstacleCategory ->
                    playerObject -- Just block player, back to previous position

                NpcCategory _ ->
                    playerObject

                CrateCategory _ ->
                    playerObject

                _ ->
                    newPlayerObject

    in
        if List.isEmpty collidingObjects then
            newPlayerObject
        else
            List.foldl updater playerObject collidingObjects




-- RENDERING


renderLevel : Resources -> Mat4 -> Level -> List Entity
renderLevel resources cameraProj level =
    level.layers
        |> List.filter .visible
        |> List.indexedMap
            (\index layer ->
                -- TODO Assign z value from Tiled editor
                renderLayer resources cameraProj layer (toFloat index / 10)
            )


renderLayer : Resources -> Mat4 -> Layer -> Float -> Entity
renderLayer resources cameraProj layer zPosition =
    let
        ( x, y ) =
            Vector2.toTuple layer.position

        position =
            vec3 x y zPosition

        atlas =
            Resources.getTexture tileSet.name resources

        lut =
            Resources.getTexture layer.lutName resources

        ( atlasW, atlasH ) =
            Texture.size atlas

        uniforms =
            { transform = Render.makeTransform position layer.size 0 ( 0.5, 0.5 )
            , cameraProj = cameraProj
            , atlas = atlas
            , lut = lut
            , atlasSize = Vector2.fromInt atlasW atlasH
            , layerSize = layer.size
            , tileSize = tileSize
            }
    in
        Render.toEntity (TiledRect uniforms)


{-| Render objects while discarding "invisibles" (obstacles and triggers)
-}
renderObjects : Float -> Mat4 -> Dict Int Object -> List Entity
renderObjects time cameraProj objects =
    let
        renderer : Int -> Object -> (List Entity -> List Entity)
        renderer id object =
            case object.category of
                PlayerCategory player ->
                    (::) (Player.render time cameraProj object player)

                CrateCategory crate ->
                    (::) (Crate.render time cameraProj object crate)

                NpcCategory npc ->
                    (::) (Npc.render time cameraProj object npc)

                _ ->
                    identity
    in
        Dict.foldl renderer [] objects
