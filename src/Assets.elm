module Assets exposing (..)

import Dict exposing (Dict)
import WebGL.Texture as Texture exposing (Texture)
import Task


-- MISC


type Msg
    = TextureLoaded String String (Result Texture.Error Texture)
    -- TODO SoundLoaded String (Result ... Sound)


{- A game asset (texture, audio clip, etc.) to be loaded
-}
type alias AssetDescription =
    { name: String
    , url : String
    }


type alias Assets =
    Dict String Texture


{-| All the levels share the same tile set
-}
tileSet =
    { name = "City"
    , url = "images/mage-city-tileset.png"
    }


player =
    { name = "Player"
    , url = "images/player.png"
    }


npc =
    { name = "Npc"
    , url = "images/npc-1.png"
    }


misc =
    { name = "Misc"
    , url = "images/misc.png"
    }


font =
    { name = "Font"
    , url = "images/font.png"
    }


all : List AssetDescription
all =
    [ tileSet
    , player
    , npc
    , misc
    , font
    ]


textureOptions =
    let
        -- https://github.com/elm-lang/elm-compiler/issues/1149
        defaultOptions = Texture.defaultOptions
    in
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearestMipmapNearest
        }


{-| Load a list of assets from the given urls. Texure filtering is set to nearest
to have a retro "pixelated" look). PNGs and JPEGs are known to work. WebGL requires
that your textures have a dimension with a power of two, e.g. 2^n x 2^m
-}
loadAssets: List AssetDescription -> Cmd Msg
loadAssets assets =
    assets
        |> List.map
            (\asset ->
                let
                    { name, url } = asset
                in
                    Task.attempt (TextureLoaded name url)
                        (Texture.loadWith textureOptions url)
            )
        |> Cmd.batch


isLoadingComplete : List AssetDescription -> Assets -> Bool
isLoadingComplete aa assets =
    Dict.size assets == List.length aa


update : Msg -> Assets -> Assets
update (TextureLoaded name url result) assets =
    case result of
        Ok texture ->
            Dict.insert ( Debug.log "Loaded texture" name ) texture assets

        Err error ->
            assets
                |> Debug.log ("Could not load texture: " ++ toString url ++ "\n" ++ toString error)



-- Hardcoded 2x2 fallback texture as data URI
notFoundTexture =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAA6RpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNy0wNC0yNVQwOTowNDo3NzwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxtYXRvciAzLjY8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6Q29tcHJlc3Npb24+NTwvdGlmZjpDb21wcmVzc2lvbj4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MjwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzI8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyPC90aWZmOlhSZXNvbHV0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MjwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOkNvbG9yU3BhY2U+MTwvZXhpZjpDb2xvclNwYWNlPgogICAgICAgICA8ZXhpZjpQaXhlbFlEaW1lbnNpb24+MjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgombEt2AAAAE0lEQVQIHWNgYGD49/8/GDMwAAAp1AX5IA3ajwAAAABJRU5ErkJggg=="


texture : String -> Assets -> Texture
texture name assets =
    case Dict.get name assets of

        Just texture ->
            texture

        Nothing ->
            -- TODO grab notFoundTexture instead
            Debug.crash ("Could not find texture " ++ name)
