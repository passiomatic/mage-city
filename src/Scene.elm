module Scene exposing
    ( spawnObjects
    , renderLevel
    , renderObject
    )

{-| Scene creation and rendering functions
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


-- CREATION


spawnObjects : Resources -> Level -> List Object
spawnObjects resources level =
    let
        spawner = (\placeholder objects ->
            case spawnObject resources placeholder of
                Just object ->
                    object :: objects

                Nothing ->
                    objects
            )
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
            Just ( Crate.spawn resources placeholder.id placeholder.name position )

        -- ("PathCategory", PolygonGeometry { points } ) ->
        --     Just ()

        _ ->
            Debug.log ( "Could not spawn object " ++ placeholder.categoryName ) Nothing


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


{-| Render an object while discarding obstacles and triggers
-}
renderObject : Float -> Mat4 -> Object -> (List Entity -> List Entity)
renderObject time cameraProj object =

    case object.category of
        ObstacleCategory ->
            identity

        TriggerCategory ->
            identity

        CrateCategory crate ->
            (::) ( Crate.render time cameraProj object crate )
