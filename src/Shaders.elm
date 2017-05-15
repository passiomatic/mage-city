module Shaders exposing
    ( texturedRectVertexShader
    , tiledRectVertexShader
    , texturedRectFragmentShader
    , coloredRectFragmentShader
    , tiledRectFragmentShader
    , animatedRectFragmentShader
    )

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Math.Matrix4 exposing (Mat4)

import WebGL exposing (Texture, Shader)


{-| Can be generally used if the fragment shader needs to display texture(s).
-}
--texturedRectVertexShader : Shader Vertex { transform : Mat4, cameraProj : Mat4 } { vcoord : Vec2 }
texturedRectVertexShader =
    [glsl|
attribute vec2 position;

uniform mat4 transform;
uniform mat4 cameraProj;

// Value passed to fragment shader

varying vec2 vcoord;

void main() {

    // Final screen vertex position
    vec4 screenPosition = cameraProj * transform * vec4(position, 0, 1);
    gl_Position = screenPosition;

    // Pass coordinates to fragment shader
    vcoord = position.xy;
}
|]


{-| Color the whole area in a single color. Mainly used while prototyping.
-}
-- coloredRectFragmentShader : Shader {} { u | color : Vec3 } { vcoord : Vec2 }
coloredRectFragmentShader =
    [glsl|
precision mediump float;

uniform vec3 color;

varying vec2 vcoord; // Unused

void main() {
    gl_FragColor = vec4(color, 1);
}
|]


{-| Render a sprite using a portion of texture atlas.
-}
-- texturedRectFragmentShader : Shader {} { u | atlas : Texture, atlasSize: Vec2, spriteSize: Vec2, spriteIndex : Float } { vcoord : Vec2 }
texturedRectFragmentShader =
    [glsl|
precision mediump float;

uniform sampler2D atlas;
uniform vec2 atlasSize;
uniform vec2 spriteSize;
uniform int spriteIndex;

// Incoming values from vertex shader

varying vec2 vcoord;

void main() {

    // Recast as float
    float spriteIndex = float(spriteIndex);

    float w = atlasSize.x;
    float h = atlasSize.y;

    // Normalize sprite size (0.0-1.0)
    float dx = spriteSize.x / w;
    float dy = spriteSize.y / h;

    // Figure out the atlas cols
    float cols = w / spriteSize.x;

    // From linear index to row/col pair
    float col = mod(spriteIndex, cols);
    float row = floor(spriteIndex / cols);

    // Finally to UV texture coordinates
    vec2 uv = vec2(dx * vcoord.x + col * dx, 1.0 - dy - row * dy + dy * vcoord.y);

    gl_FragColor = texture2D(atlas, uv);
}
|]


{- Render a portion of a tileset using another texture as look up table (LUT).
See: http://blog.tojicode.com/2012/07/sprite-tile-maps-on-gpu.html
-}
-- vertexTileset : Shader Vertex { u
--     | transform : Mat4
--     , cameraProj : Mat4
--     , layerSize : Vec2
-- } { pixelCoord : Vec2,  textureCoord : Vec2 }
tiledRectVertexShader =
    [glsl|
precision mediump float;

attribute vec2 position;

uniform mat4 transform;
uniform mat4 cameraProj;
uniform vec2 layerSize;

// Values passed to fragment shader

varying vec2 vcoord;
varying vec2 vlutCoord;

void main () {

    // Final vertex position
    vec4 screenPosition = cameraProj * transform * vec4(position, 0, 1);
    gl_Position = screenPosition;

    // Pass coordinates to fragment shader
    vcoord = position.xy * layerSize;
    vlutCoord = position.xy;
}
|]

{- Render a tile from a texture atlas using another texture as look up table (LUT).
-}
tiledRectFragmentShader : Shader {} { u |
    atlas : Texture,
    lut : Texture,
    atlasSize : Vec2,
    tileSize: Vec2 } { vcoord: Vec2, vlutCoord: Vec2 }
tiledRectFragmentShader =
    [glsl|
precision mediump float;

uniform sampler2D atlas;
uniform sampler2D lut;

uniform vec2 atlasSize;
uniform vec2 tileSize;

// Incoming values from vertex shader

varying vec2 vcoord;
varying vec2 vlutCoord;

void main () {
    vec4 tile = texture2D(lut, vlutCoord);

    // If both R and G channels are 1.0 then the tile is empty
    if(tile.x == 1.0 && tile.y == 1.0) {
        discard;
    }

    // Denormalized sprite offset in atlas
    vec2 spriteOffset = floor(tile.xy * 256.0) * tileSize;
    vec2 delta = mod(vcoord, tileSize);

    // Flip Y axis
    spriteOffset.y = atlasSize.y - spriteOffset.y - tileSize.y;

    // Final normalized sprite position
    vec2 spritePosition = (spriteOffset + delta) / atlasSize;

    gl_FragColor = texture2D(atlas, spritePosition);

    // Discard the transparent color
    if (gl_FragColor.a == 0.0) {
      discard;
    }
}
|]


{-  Render atlas animations. It assumes that the animation frames are in one horizontal line.
-}
-- animatedRectFragmentShader : Shader {} { u | atlas : Texture, atlasSize : Vec2, spriteSize : Vec2, spriteIndex : Int, frameCount : Int, duration : Float, time : Float } { vcoord : Vec2 }
animatedRectFragmentShader =
    [glsl|
precision mediump float;

uniform sampler2D atlas;
uniform vec2 spriteSize;
uniform vec2 atlasSize;
uniform int spriteIndex;

uniform int frameCount; // Number of frames
uniform float duration; // Total duration
uniform float time;

// Incoming value from vertex shader

varying vec2 vcoord;

void main () {

    // Recast as float
    float frameCount = float(frameCount);
    float spriteIndex = float(spriteIndex);

    // Frame index in time
    float frameIndex = floor(mod(time, duration) / duration * frameCount);

    // Normalize sprite size (0.0-1.0)
    vec2 unitSpriteSize = spriteSize / atlasSize;

    // From linear index (0-...) to actual row
    float row = floor(spriteIndex / (atlasSize.x / spriteSize.x));

    // Finally to UV texture coordinates
    vec2 uv = vec2(
        frameIndex * unitSpriteSize.x + unitSpriteSize.x * vcoord.x,
        // Flip Y axis
        1.0 - unitSpriteSize.y - row * unitSpriteSize.y + unitSpriteSize.y * vcoord.y);

    gl_FragColor = texture2D(atlas, uv);

}
|]
