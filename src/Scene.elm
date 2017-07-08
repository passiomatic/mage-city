module Scene exposing
    ( spawnObjects
    , updateObjects
    , renderObjects
    , renderLevel
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
import Render exposing (makeTransform, toEntity, Uniform(..))
import Object exposing (Object, Category(..))
import Crate
import Npc


-- CREATION


spawnObjects : Resources -> Level -> List Object
spawnObjects resources level =

    let
        spawner : Placeholder -> (List Object -> List Object)
        spawner placeholder =
            case spawnObject resources placeholder of
                Just object ->
                    (::) object

                Nothing ->
                    identity
    in
        List.foldl spawner [] level.placeholders


spawnObject : Resources -> Placeholder -> Maybe Object
spawnObject resources placeholder =

    case (placeholder.categoryName, placeholder.geometry) of

        ( "TriggerCategory", RectangleGeometry { position, size } ) ->
            Just
                { category = TriggerCategory
                , id = placeholder.id
                , name = placeholder.name
                , position = position
                , collisionSize = size
                }

        ( "ObstacleCategory", RectangleGeometry { position, size } ) ->
            Just
                { category = ObstacleCategory
                , id = placeholder.id
                , name = placeholder.name
                , position = position
                , collisionSize = size
                }

        ( "CrateCategory", RectangleGeometry { position } ) ->
            --Just ( Debug.log "Spawn crate @ " (Crate.spawn resources placeholder.id placeholder.name position) )
            Just ( Crate.spawn resources placeholder.id placeholder.name position )

        ( "NpcCategory", RectangleGeometry { position } ) ->
            Just ( Npc.spawn resources placeholder.id placeholder.name position )

        -- ("PathCategory", PolygonGeometry { points } ) ->
        --     Just ()

        _ ->
            Debug.log ( "Could not spawn object " ++ placeholder.categoryName ) Nothing

-- UPDATING


{-| Update all the scene objects.
-}
updateObjects dt objects =

    let
        updater : Object -> (List Object -> List Object)
        updater object =

            case object.category of
                NpcCategory npc ->
                    (::) ( Npc.tick dt object npc )

                _ ->
                    -- Return the orginal object
                    (::) object
    in
        List.foldl updater [] objects




-- RENDERING


renderLevel : Resources -> Mat4 -> Level -> List Entity
renderLevel resources cameraProj level =
    level.layers
        |> List.filter (\layer -> layer.visible)
        |> List.indexedMap (\index layer ->
            -- TODO Assign z value from Tiled editor
            renderLayer resources cameraProj layer (toFloat index / 10)
        )


renderLayer : Resources -> Mat4 -> Layer -> Float -> Entity
renderLayer resources cameraProj layer zPosition =
    let
        position = Vector3.fromVec2 layer.position zPosition

        atlas =
            Resources.getTexture tileSet.name resources

        lut =
            Resources.getTexture layer.lutName resources

        (atlasW, atlasH) =
            Texture.size atlas

        uniforms =
            { transform = makeTransform position layer.size 0 (0.5, 0.5)
            , cameraProj = cameraProj
            , atlas = atlas
            , lut = lut
            , atlasSize = Vector2.fromInt atlasW atlasH
            , layerSize = layer.size
            , tileSize = tileSize
            }
    in
        toEntity (TiledRect uniforms)


{-| Render objects while discarding "invisibles" (obstacles and triggers)
-}
renderObjects time cameraProj objects =

    let
        renderer : Object -> (List Entity -> List Entity)
        renderer object =

            case object.category of
                CrateCategory crate ->
                    (::) (Crate.render time cameraProj object crate)

                NpcCategory npc ->
                    (::) ( Npc.render time cameraProj object npc )

                _ ->
                    identity
    in
         List.foldl renderer [] objects
