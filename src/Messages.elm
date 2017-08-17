module Messages exposing (..)

import Window exposing (Size)
import WebGL.Texture exposing (Error, Texture)
import Keyboard.Extra as Keyboard
import Tiled exposing (Level)
import WebGL.Texture as Texture exposing (Texture)
import Assets

type Msg
    = ScreenSize Window.Size
    | Tick Float
    | KeyMsg Keyboard.Msg
    | AssetMsg Assets.Msg
      -- REMOVEME Game messages
    | ChangeLevel Level
