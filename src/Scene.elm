module Scene
    exposing
        ( spawnObjects
        , update
        , render
        , resolveCollisions
        )

{-| Scene creation, updating and rendering functions
-}

import Assets exposing (Assets)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Vector2Extra as Vector2
import Vector3Extra as Vector3
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import Tiled exposing (Level, Layer, Placeholder, Geometry(..), tileSize)
import Render exposing (Uniform(..))
import Objects.Object as Object exposing (Object, Player, Npc, Category(..))
import Objects.Player as Player
import Objects.Crate as Crate
import Objects.Npc as Npc
import Dict exposing (Dict)
import Keyboard.Extra as Keyboard exposing (Direction(..))
import Assets
import Text
import Model exposing (Model)
import Camera


-- CREATION


spawnObjects : Assets -> Dict Int Placeholder -> Dict Int Object
spawnObjects assets placeholders =
    let
        spawner : Int -> Placeholder -> (Dict Int Object -> Dict Int Object)
        spawner _ placeholder =
            case spawnObject assets placeholder of
                Just object ->
                    Dict.insert object.id object

                Nothing ->
                    identity
    in
        Dict.foldl spawner Dict.empty placeholders


spawnObject : Assets -> Placeholder -> Maybe Object
spawnObject assets ({ categoryName, id, name, geometry } as placeholder) =
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
            Just (Crate.spawn assets id name position)

        ( "Npc", RectangleGeometry { position } ) ->
            Just (Npc.spawn assets id name position)

        ( "Player", RectangleGeometry { position } ) ->
            Just (Player.spawn assets position)

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


fontAsset =
    Assets.font


tileSetAsset =
    Assets.tileSet


render : Model -> List Entity
render ( { assets, time, viewport, level, objects, camera, uiCamera } as model ) =
    let
        cameraProj =
            Camera.view viewport camera

        uiCameraProj =
            Camera.view viewport uiCamera

        atlas =
            Assets.texture fontAsset.name assets
    in
        renderLayers assets cameraProj level
            ++ renderObjects time cameraProj objects
            ++ Text.renderText time uiCameraProj (vec3 -30 -150 0.8) atlas "42/99"


renderLayers : Assets -> Mat4 -> Level -> List Entity
renderLayers assets cameraProj level =
    let
        atlas =
            Assets.texture tileSetAsset.name assets

        ( atlasW, atlasH ) =
            Texture.size atlas

        renderer : Layer -> Entity
        renderer layer =
            let
                lut =
                    Assets.texture layer.name assets

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
