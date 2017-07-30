module Assets exposing (..)

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

all =
    [ tileSet
    , player
    , npc
    , misc
    ]
