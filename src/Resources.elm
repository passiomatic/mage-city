module Resources exposing
    ( Msg
    , Resources
    , Asset
    , initialModel
    , loadAssets
    , getTexture
    , update
    , isLoadingComplete
    )

import Dict exposing (Dict)
import Task
import WebGL.Texture as Texture exposing (Texture)


{-| -}
type Msg
    = AssetLoaded String String (Result Texture.Error Texture)
    -- TODO | SoundLoaded String (Result ... Sound)


{- A game asset (texture, audio clip, etc.) to be loaded
-}
type alias Asset = -- TODO change in  AssetDescription
    { name: String
    , url : String
    }

{-|
The main type of this library
-}
type Resources -- TODO Rename is Textures ???
    = R (Dict String Texture)
    -- TODO | Sound (Dict String Sound)
    -- | NotFound texture


{-| -}
initialModel : Resources
initialModel =
    -- Add notFoundTexture
    R Dict.empty

{-
Loads a list of textures from the given urls. Texure filtering is set to nearest
to have a retro "pixelated" look). PNGs and JPEGs are known to work. WebGL requires
that your textures have a dimension with a power of two, e.g. 2^n x 2^m
-}
-- loadTextures : List String -> Cmd Msg
-- loadTextures urls =
--     let
--         -- https://github.com/elm-lang/elm-compiler/issues/1149
--         defaultOptions = Texture.defaultOptions
--         options = { defaultOptions
--             | magnify = Texture.nearest
--             , minify = Texture.nearestMipmapNearest
--         }
--     in
--         urls
--             |> List.map
--                 (\url ->
--                     Task.attempt (LoadedTexture url)
--                         (Texture.loadWith options url)
--                 )
--             |> Cmd.batch

textureOptions =
    let
        -- https://github.com/elm-lang/elm-compiler/issues/1149
        defaultOptions = Texture.defaultOptions
    in
        { defaultOptions
            | magnify = Texture.nearest
            , minify = Texture.nearestMipmapNearest
            --, flipY = False
        }

{-| Load a list of assets from the given urls. Texure filtering is set to nearest
to have a retro "pixelated" look). PNGs and JPEGs are known to work. WebGL requires
that your textures have a dimension with a power of two, e.g. 2^n x 2^m
-}
loadAssets: List Asset -> Cmd Msg
loadAssets assets =
    assets
        |> List.map
            (\asset ->
                let
                    { name, url } = asset
                in
                    Task.attempt (AssetLoaded name url)
                        (Texture.loadWith textureOptions url)
            )
        |> Cmd.batch


isLoadingComplete : List Asset -> Resources -> Bool
isLoadingComplete aa (R assets) =
    Dict.size assets == List.length aa


update : Msg -> Resources -> Resources
update (AssetLoaded name url result) (R assets) =
    case result of
        Ok texture ->
            R (Dict.insert (Debug.log "Loaded texture" name ) texture assets)
            --R (Dict.insert name texture assets)

        Err error ->
            R assets
                |> Debug.log ("Could not load texture: " ++ toString url ++ "\n" ++ toString error)



-- Hardcoded 2x2 fallback texture as data URI
notFoundTexture =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAIAAAD91JpzAAAAAXNSR0IArs4c6QAAAAlwSFlzAAALEwAACxMBAJqcGAAAA6RpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyIKICAgICAgICAgICAgeG1sbnM6ZXhpZj0iaHR0cDovL25zLmFkb2JlLmNvbS9leGlmLzEuMC8iPgogICAgICAgICA8eG1wOk1vZGlmeURhdGU+MjAxNy0wNC0yNVQwOTowNDo3NzwveG1wOk1vZGlmeURhdGU+CiAgICAgICAgIDx4bXA6Q3JlYXRvclRvb2w+UGl4ZWxtYXRvciAzLjY8L3htcDpDcmVhdG9yVG9vbD4KICAgICAgICAgPHRpZmY6T3JpZW50YXRpb24+MTwvdGlmZjpPcmllbnRhdGlvbj4KICAgICAgICAgPHRpZmY6Q29tcHJlc3Npb24+NTwvdGlmZjpDb21wcmVzc2lvbj4KICAgICAgICAgPHRpZmY6UmVzb2x1dGlvblVuaXQ+MjwvdGlmZjpSZXNvbHV0aW9uVW5pdD4KICAgICAgICAgPHRpZmY6WVJlc29sdXRpb24+NzI8L3RpZmY6WVJlc29sdXRpb24+CiAgICAgICAgIDx0aWZmOlhSZXNvbHV0aW9uPjcyPC90aWZmOlhSZXNvbHV0aW9uPgogICAgICAgICA8ZXhpZjpQaXhlbFhEaW1lbnNpb24+MjwvZXhpZjpQaXhlbFhEaW1lbnNpb24+CiAgICAgICAgIDxleGlmOkNvbG9yU3BhY2U+MTwvZXhpZjpDb2xvclNwYWNlPgogICAgICAgICA8ZXhpZjpQaXhlbFlEaW1lbnNpb24+MjwvZXhpZjpQaXhlbFlEaW1lbnNpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgombEt2AAAAE0lEQVQIHWNgYGD49/8/GDMwAAAp1AX5IA3ajwAAAABJRU5ErkJggg=="


getTexture : String -> Resources -> Texture
getTexture name (R assets) =
    case Dict.get name assets of

        Just texture ->
            texture

        Nothing ->
            -- TODO grab notFoundTexture instead
            Debug.crash ("Could not find texture " ++ name)
