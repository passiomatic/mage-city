module Model
    exposing
        ( Model
        , GameState(..)
        )

import Keyboard.Extra as Keyboard exposing (Key)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Tiled exposing (Level)
import Camera exposing (Camera)
import Objects.Object exposing (Object)
import Levels.Forest1 as Forest1
import Dict exposing (Dict)
import Assets exposing (Assets)


type GameState
    = Loading
    | Playing


type alias Model =
    { objects: Dict Int Object
    , assets : Assets
    , pressedKeys : List Key
    , time : Float
    , viewport : Vec2
    , camera : Camera
    , uiCamera : Camera
    , level : Level
    , state : GameState
    }
