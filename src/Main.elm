module Main exposing (..)

import Html exposing (Html, text)
import Task
import AnimationFrame
import Window
import Keyboard.Extra
import Color exposing (Color)
import Resources as Resources exposing (Resources, Asset)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import WebGL.Texture as Texture exposing (Texture)
import Camera
--import WebGLView
import Render
import Player exposing (Player)
import Tiled exposing (Level, tileSet, Obstacle)
import Camera exposing (Camera)
import GameEntity exposing (GameEntity)
import Levels.Forest1 as Forest1


-- All the game levels
levels =
    [ Forest1.level
    ]


startLevel =
    Forest1.level


-- Old school 4:3 aspect ratio
viewportSize =
    vec2 400 300


vieportScale =
    2.0


-- MSG

type Msg
    = ScreenSize Window.Size
    | Tick Float
    | Resources Resources.Msg
    | Keys Keyboard.Extra.Msg
      -- Game messages
    | ChangeLevel Level



type alias Model =
    { player : Player
    , gameEntities: List GameEntity
    , resources : Resources
    , keys : Keyboard.Extra.State
    , time : Float
    , viewport : Vec2
    , camera : Camera
    , level : Level
    , state : GameState
    }


type GameState
    = Loading
    | Playing


init : ( Model, Cmd Msg )
init =
    { player = Player.initialModel
    , gameEntities = []
    , resources = Resources.initialModel
    , keys = Keyboard.Extra.initialState
    , time = 0
    , viewport = viewportSize
    , camera = Camera.fixedArea (Vector2.getX viewportSize * Vector2.getY viewportSize) (vec2 200 150)
    , level = startLevel
    , state = Loading
    }
        ! [ getScreenSize
          , Cmd.map Resources (Resources.loadAssets gameAssets)
          ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLevel level ->
            { model
                | gameEntities = spawnGameEntities model.resources level
                , level = level
            }
                ! []

        ScreenSize { width, height } ->
            --{ model | screen = ( width, height ) } ! []
            model ! []

        Tick dt ->
            { model
                | player = tick dt model.keys model.level.obstacles model.player
                , time = dt + model.time
                , camera = updateCamera dt model.player.position model.camera
            }
                ! []

        Resources msg ->
            let
                newResources =
                    Resources.update msg model.resources
            in
                if Resources.isLoadingComplete gameAssets newResources then
                    { model
                        | resources = newResources
                        , state = Playing
                    }
                        -- Trigger a ChangeLevel msg
                        -- See: https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
                        |>
                            update (ChangeLevel startLevel)
                else
                    { model
                        | resources = newResources
                    }
                        ! []

        Keys keyMsg ->
            let
                keys =
                    Keyboard.Extra.update keyMsg model.keys
            in
                { model
                    | keys = keys
                }
                    ! []


spawnGameEntities resources level =
    List.map (\object -> GameEntity.spawn resources object.type_ object.position object.size) level.spawns


minDistanceFromEdge =
    70


updateCamera : Float -> Vec2 -> Camera -> Camera
updateCamera dt targetPosition camera =
    Camera.follow 0.95 dt (nextCameraPosition camera.position targetPosition) camera


nextCameraPosition cameraPosition targetPosition =

    let
        (w, h) = Vector2.toTuple viewportSize

        posX = Vector2.getX cameraPosition
        posY = Vector2.getY cameraPosition

        -- Rel pos
        (x, y) =
            --Debug.log "x,y" (relativeTo cameraPosition  viewportSize targetPosition |> Vector2.toTuple)
            relativeTo cameraPosition viewportSize targetPosition |> Vector2.toTuple

        -- Check if on west/east edge
        newX = if x < minDistanceFromEdge then
            posX - minDistanceFromEdge
        else if x > (w - minDistanceFromEdge) then
            posX + minDistanceFromEdge
        else
            posX

        -- Check if on north/south edge
        newY = if y < minDistanceFromEdge then
            posY - minDistanceFromEdge
        else if y > (h - minDistanceFromEdge) then
            posY + minDistanceFromEdge
        else
            posY
    in
        vec2 newX newY


relativeTo referencePosition referenceSize position =
    let
        size = Vector2.scale 0.5 referenceSize
    in
        referencePosition
            |> Vector2.sub position
            |> Vector2.add size



tick : Float -> Keyboard.Extra.State -> List Obstacle -> Player -> Player
tick dt keys obstacles player =
    Player.tick dt keys obstacles player



-- All the game assets (images, sounds, etc.)


gameAssets =
    let
        assets =
            [ List.singleton ( tileSet.name, tileSet.url )
            , Player.assets
            , GameEntity.assets
            ]
    in
        -- Add all the assets from levels
        assets
            |> List.append (List.map (\level -> level.assets) levels)
            |> List.concat



-- VIEW


renderPlaying : Model -> Html msg
renderPlaying { player, gameEntities, resources, time, viewport, camera, level } =
    let
        cameraProj =
            Camera.view viewport camera

        renderGameEntities =
            List.map (\gameEntity -> GameEntity.render resources time cameraProj gameEntity) gameEntities

        scene =
            Tiled.renderLevel resources cameraProj level
                ++ renderGameEntities
                ++ [ Player.render resources time cameraProj player ]

        -- Calculate scaled WebGL canvas size
        ( w, h ) =
            Vector2.scale vieportScale viewport |> Vector2.toTuple
    in
        Render.toWebGL ( floor w, floor h ) scene



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
        , Sub.map Keys Keyboard.Extra.subscriptions
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
