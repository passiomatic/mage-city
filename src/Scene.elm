module Scene
    exposing
        ( spawnObjects
        , update
        , render
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
import Tiled exposing (Level, Layer, Placeholder, Geometry(..), tileSize)
import Render exposing (Uniform(..))
import Object exposing (Object, Player, Npc, Category(..))
import Player
import Crate
import Npc
import Dict exposing (Dict)
import Keyboard.Extra as Keyboard exposing (Direction(..))
import Assets
import Model exposing (Model)


-- CREATION


spawnObjects : Resources -> Dict Int Placeholder -> Dict Int Object
spawnObjects resources placeholders =
    let
        spawner : Int -> Placeholder -> (Dict Int Object -> Dict Int Object)
        spawner _ placeholder =
            case spawnObject resources placeholder of
                Just object ->
                    Dict.insert object.id object

                Nothing ->
                    identity
    in
        Dict.foldl spawner Dict.empty placeholders


spawnObject : Resources -> Placeholder -> Maybe Object
spawnObject resources ({ categoryName, id, name, geometry } as placeholder) =
    case ( categoryName, geometry ) of
        ( "Trigger", RectangleGeometry { position, size } ) ->
            Just
                { category = TriggerCategory
                , id = id
                , name = name
                , position = position
                , collisionSize = size
                , collisionCategory = Object.collisionTriggerCategory
                , collisionBitMask = 0
                }

        ( "Obstacle", RectangleGeometry { position, size } ) ->
            Just
                { category = ObstacleCategory
                , id = id
                , name = name
                , position = position
                , collisionSize = size
                , collisionCategory = Object.collisionObstacleCategory
                , collisionBitMask = 0
                }

        ( "Crate", RectangleGeometry { position } ) ->
            Just (Crate.spawn resources id name position)

        ( "Npc", RectangleGeometry { position } ) ->
            Just (Npc.spawn resources id name position)

        ( "Player", RectangleGeometry { position } ) ->
            Just (Player.spawn resources position)

        _ ->
            Debug.log ("Cannot spawn object with category " ++ categoryName ++ ", skipped") Nothing



-- UPDATING


{-| Update all the scene objects
-}
update : Model -> Dict Int Object -> Dict Int Object
update model objects =
    let
        updater : Int -> Object -> Object
        updater id object =
            case object.category of
                PlayerCategory player ->
                    Player.update model object player

                NpcCategory npc ->
                    Npc.update model object npc

                _ ->
                    -- Just return the original object
                    object
    in
        Dict.map updater model.objects


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

                NpcCategory npc ->
                    resolveNpcCollisions dt object npc objects

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
            Object.colliding (Dict.values objects) newPlayerObject

        updater : Object -> Object -> Object
        updater otherObject playerObject =
            case otherObject.category of
                ObstacleCategory ->
                    playerObject -- Just block player, pass previous position

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


resolveNpcCollisions : Float -> Object -> Npc -> Dict Int Object -> Object
resolveNpcCollisions dt npcObject npc objects =

    let
        newNpcObject =
            Object.move dt npc.velocity npcObject

        collidingObjects =
            Object.colliding (Dict.values objects) newNpcObject

        updater : Object -> Object -> Object
        updater otherObject playerObject =
            case otherObject.category of
                ObstacleCategory ->
                    npcObject -- Just block NPC, pass previous position

                PlayerCategory _ ->
                    npcObject

                _ ->
                    newNpcObject

    in
        if List.isEmpty collidingObjects then
            newNpcObject
        else
            List.foldl updater npcObject collidingObjects


-- RENDERING


render : Resources -> Float -> Mat4 -> Level -> Dict Int Object -> List Entity
render resources time cameraProj level objects =
    renderLayers resources cameraProj level
        ++ renderObjects time cameraProj objects


renderLayers : Resources -> Mat4 -> Level -> List Entity
renderLayers resources cameraProj level =
    let
        tileSetAsset =
            Assets.tileSet

        atlas =
            Resources.getTexture tileSetAsset.name resources

        ( atlasW, atlasH ) =
            Texture.size atlas

        renderer : Layer -> Entity
        renderer layer =
            let
                lut =
                    Resources.getTexture layer.name resources

                uniforms =
                    { transform = Render.makeTransform layer.position layer.size 0 ( 0.5, 0.5 )
                    , cameraProj = cameraProj
                    , atlas = atlas
                    , lut = lut
                    , atlasSize = Vector2.fromInt atlasW atlasH
                    , layerSize = layer.size
                    , tileSize = tileSize
                    }
            in
                Render.toEntity (TiledRect uniforms)
    in
        level.layers
            -- |> List.filter .isVisible -- TODO Consider layer visibility
            |> List.map renderer


{-| Render objects while discarding "invisibles" (obstacles and triggers)
-}
renderObjects : Float -> Mat4 -> Dict Int Object -> List Entity
renderObjects time cameraProj objects =
    let
        renderer : Int -> Object -> (List Entity -> List Entity)
        renderer id object =
            case object.category of
                PlayerCategory player ->
                    (++) (Player.render time cameraProj object player)

                NpcCategory npc ->
                    (++) (Npc.render time cameraProj object npc)

                CrateCategory crate ->
                    (::) (Crate.render time cameraProj object crate)

                _ ->
                    identity
    in
        Dict.foldl renderer [] objects
