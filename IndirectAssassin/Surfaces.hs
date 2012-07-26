module IndirectAssassin.Surfaces where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Keysym
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map
import IndirectAssassin.Logic
import IndirectAssassin.Types

floor = SDLi.load "floor.png" 
wall  = SDLi.load "wall.png"
agent         = walkcycle  9 4  50 "character/agent.png"
professor     = walkcycle  9 4  50 "character/professor.png"
soldierZombie = walkcycle  9 4  50 "character/soldier_zombie.png"
soldierNormal = walkcycle  9 4  50 "character/soldier_normal.png"
barrels       = still              "barrels.png"
buckets       = still              "buckets.png"
bat           = animation  3 4  80 "bat_yellow.png"
bee           = animation  3 4  80 "bee_green.png"
diamond       = animation  4 1 120 "diamond.png"
iceShield     = animation  4 4  60 "ice_shield.png"
tomato        = animation 16 2  40 "tomato.png"


walkcycle :: Int -> Int -> Int -> String -> Direction -> Word32 -> IO SurfPart
walkcycle xTiles yTiles frameDur path direc i = (surf, rect)
  where rect = SDL.Rect (x * tileW) (y * tileH) tileW tileH
        (x, y) = (n `rem` xTiles, floor $ n / xTiles)
        n = nOffset + floor $ fromIntegral oneDir * i / fps
        nOffset = oneDir * fromEnum direc
        oneDir = floor $ tileW * tileH / 4
        (tileW, tileH) = (floor $ w / xTiles, floor $ h / yTiles)
        (w, h) = (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf)
        surf = SDLi.load path

animation :: Int -> Int -> Int -> String -> Word32 -> IO SurfPart
animation xTiles yTiles frameDur path = walkcycle xTiles (4 * yTiles) frameDur path $ toEnum 0

still :: String -> Word32 -> IO SurfPart
still path _ = (surf, SDL.getSize surf)
  where surf = SDLi.load path
