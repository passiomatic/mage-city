module Text exposing (wrapText, renderText)

{-| Text rendering
-}

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Vector3Extra as Vector3
import Vector2Extra as Vector2

import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import Char
import Shaders
import Render


gliphWidth =
    12


spriteSize =
    vec2 16 16


startCode =
    32 -- First ASCII code mapped


renderGlyph : Float -> Mat4 -> Vec3 -> Texture -> Char -> Entity
renderGlyph time cameraProj position atlas glyph =
    let
        code =
            Char.toCode glyph

        ( atlasW, atlasH ) =
            Texture.size atlas

        uniforms : Render.TexturedRectUniform
        uniforms =
            -- Move glyph origin to bottom-left corner
            { transform = Render.makeTransform position spriteSize 0 ( 0.0, 0.0 )
            , cameraProj = cameraProj
            , atlas = atlas
            , atlasSize = Vector2.fromInt atlasW atlasH
            , spriteSize = spriteSize
            , spriteIndex = code - startCode
            }
    in
        Render.toEntity (Render.TexturedRect uniforms)


renderText : Float -> Mat4 -> Vec3 -> Texture -> String -> List Entity
renderText time cameraProj position atlas text =
    let
        (x, y, z) =
            Vector3.toTuple position

        renderer : Int -> Char -> Entity
        renderer index glyph =
            let
                newPosition =
                    vec3 (x + gliphWidth * toFloat index) y (z + (toFloat index / 100.0))
            in
                renderGlyph time cameraProj newPosition atlas glyph
    in
        List.indexedMap renderer (String.toList text)


wrapText : Int -> String -> List String
wrapText maxWidth text =
    let
        words = String.split " " text
    in
        wrapText_ maxWidth "" words


wrapText_ : Int -> String -> List String -> List String
wrapText_ maxWidth line words =
    case words of
        [] ->
            -- Finished, do not throw away remaining line
            List.singleton line

        word :: rest ->
            let
                line_ = line ++ " " ++ word
            in
                if String.length line_ * gliphWidth < maxWidth then
                    wrapText_ maxWidth line_ rest
                else
                    -- Overflow, save incoming line and start a new one
                    line :: wrapText_ maxWidth "" words
