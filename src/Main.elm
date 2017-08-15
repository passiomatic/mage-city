module Main exposing (..)

import Html exposing (Html, text)
import Task
import AnimationFrame
import Window
import Keyboard.Extra as Keyboard
import Resources as Resources exposing (Resources, Asset)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import WebGL.Texture as Texture exposing (Texture)
import Render
import Scene
import Tiled exposing (Level)
import Camera exposing (Camera)
import Object exposing (Object)
import Crate
import Npc
import Player
import Levels.Forest1 as Forest1
--import Levels.City1 as City1
import Dict exposing (Dict)
import Assets
import Model exposing (Model, GameState(..))

levels =
    [ Forest1.level
    --, City1.level
    ]

-- TODO remove me, use Model.viewportSize
startLevel =
    Forest1.level


-- TODO remove me, use Model.viewportSize
viewportSize =
    vec2 400 300 -- Old school 4:3 aspect ratio


vieportScale =
    2.0


-- MSG

type Msg
    = ScreenSize Window.Size
    | Tick Float
    | Resources Resources.Msg
    | KeyMsg Keyboard.Msg
      -- Game messages
    | ChangeLevel Level


init : ( Model, Cmd Msg )
init =
    Model.model
        ! [ getScreenSize
          , Cmd.map Resources (Resources.loadAssets gameAssets)
          ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLevel level ->
            changeLevel level model ! []

        ScreenSize { width, height } ->
            --{ model | screen = ( width, height ) } ! []
            model ! []

        Tick dt ->
            (tick dt model) ! []

        Resources msg ->
            let
                newResources =
                    Resources.update msg model.resources
            in
                if Resources.isLoadingComplete gameAssets newResources then
                    ({ model
                        | resources = newResources
                        , state = Playing
                    }
                        |>
                            changeLevel startLevel) ! []
                else
                    { model
                        | resources = newResources
                    }
                        ! []

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }
            , Cmd.none
            )


{-| Return the player object
-}
getPlayer : Dict Int Object -> Maybe Object
getPlayer objects =
    Dict.get Player.objectId objects


changeLevel : Level -> Model -> Model
changeLevel level model =
    let
        objects =
            Scene.spawnObjects model.resources level.placeholders

        camera =
            case getPlayer objects of
                Just playerObject ->
                    -- Center camera on player
                    Camera.moveTo playerObject.position model.camera
                Nothing ->
                    model.camera
    in
    { model
        | objects = objects
        , level = level
        , camera = camera
    }


tick : Float -> Model -> Model
tick dt ( {objects, viewport, camera } as model ) =

    let
        time =
            dt + model.time

        -- Update all game objects
        newObjects =
            objects
                |> Scene.update model
                |> Scene.resolveCollisions dt

        -- Adjust camera to the resolved target position
        newCamera =
            case getPlayer newObjects of
                Just target ->
                    updateCamera dt viewport target.position camera
                Nothing ->
                    camera

    in
        { model
        | objects = newObjects
        , time = time
        , camera = newCamera
        }


-- CAMERA


minDistanceFromEdge =
    70


cameraSpeed =
    0.95


updateCamera : Float -> Vec2 -> Vec2 -> Camera -> Camera
updateCamera dt viewport targetPosition camera =
    let
        ( w, h ) =
            Vector2.toTuple viewport

        ( cameraX, cameraY ) =
            Vector2.toTuple camera.position

        ( x, y ) =
            relativePosition camera.position viewport targetPosition
                |> Vector2.toTuple

        -- Check if on west/east edge
        newX = if x < minDistanceFromEdge then
            cameraX - minDistanceFromEdge
        else if x > (w - minDistanceFromEdge) then
            cameraX + minDistanceFromEdge
        else
            cameraX

        -- Check if on north/south edge
        newY = if y < minDistanceFromEdge then
            cameraY - minDistanceFromEdge
        else if y > (h - minDistanceFromEdge) then
            cameraY + minDistanceFromEdge
        else
            cameraY
    in
        Camera.follow cameraSpeed dt (vec2 newX newY) camera


relativePosition : Vec2 -> Vec2 -> Vec2 -> Vec2
relativePosition referencePosition referenceSize position =
    let
        size = Vector2.scale 0.5 referenceSize
    in
        referencePosition
            |> Vector2.sub position
            |> Vector2.add size


-- ASSETS


-- All the game assets (images, sounds, etc.)
gameAssets =
    let
        assets =
            [ Assets.all
            ]
    in
        -- Add all the assets from levels
        assets
            |> List.append (List.map .assets levels)
            |> List.concat


-- VIEW


renderPlaying : Model -> Html msg
renderPlaying { objects, resources, time, viewport, camera, level } =
    let
        cameraProj =
            Camera.view viewport camera

        scene =
            Scene.render resources time cameraProj level objects

        -- Calculate scaled WebGL canvas size
        ( w, h ) =
            Vector2.scale vieportScale viewport
                |> Vector2.toTuple
    in
        Render.toHtml ( floor w, floor h ) scene



renderLoading : Model -> Html msg
renderLoading _ =
    text "Loading assets..."


view : Model -> Html msg
view model =
    case model.state of
        Loading ->
            renderLoading model

        Playing ->
            renderPlaying model


getScreenSize : Cmd Msg
getScreenSize =
    Task.perform ScreenSize (Window.size)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes ScreenSize
        , Sub.map KeyMsg Keyboard.subscriptions
        , AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
