module Model
    exposing
        ( Model
        , GameState(..)
        , model
        )

import Keyboard.Extra
import Resources as Resources exposing (Resources, Asset)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Vector2Extra as Vector2
import Tiled exposing (Level)
import Camera exposing (Camera)
import Object exposing (Object)
import Levels.Forest1 as Forest1
-- import Levels.City1 as City1
import Dict exposing (Dict)
--import Levels.City1 as City1


viewportSize =
    vec2 400 300 -- Old school 4:3 aspect ratio

startLevel =
    Forest1.level

type GameState
    = Loading
    | Playing


type alias Model =
    { objects: Dict Int Object
    , resources : Resources
    , keys : Keyboard.Extra.State
    , time : Float
    , viewport : Vec2
    , camera : Camera
    , level : Level
    , state : GameState
    }


model : Model
model =
    { objects = Dict.empty
    , resources = Resources.initialModel
    , keys = Keyboard.Extra.initialState
    , time = 0
    , viewport = viewportSize
    , camera = Camera.fixedArea viewportSize (vec2 250 140)
    , level = startLevel
    , state = Loading
    }
