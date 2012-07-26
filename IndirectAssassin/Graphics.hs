module IndirectAssassin.Graphics where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLttf
import Paths_IndirectAssassin (getDataFileName)
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map
import IndirectAssassin.Logic
import IndirectAssassin.Types

floorS = SDLi.load =<< getDataFileName "data/floor.png" 
wall   = SDLi.load =<< getDataFileName "data/wall.png"
font   = SDLttf.openFont =<< getDataFileName "data/embosst1.ttf"
agent         = walkcycle  9 4  50 "data/character/agent.png"
professor     = walkcycle  9 4  50 "data/character/professor.png"
soldierNormal = walkcycle  9 4  50 "data/character/soldier_normal.png"
soldierZombie = walkcycle  9 4  50 "data/character/soldier_zombie.png"
barrels       = still              "data/item/barrels.png"
buckets       = still              "data/item/buckets.png"
bat           = animation  3 4  80 "data/item/bat_yellow.png"
bee           = animation  3 4  80 "data/item/bee_green.png"
diamond       = animation  4 1 120 "data/item/diamond.png"
iceShield     = animation  4 4  60 "data/item/ice_shield.png"
tomato        = animation 16 2  40 "data/item/tomato.png"


walkcycle :: Int -> Int -> Int -> String -> Direction -> Word32 -> IO SurfPart
walkcycle xTiles yTiles frameDur path direc i = do
  path' <- getDataFileName path
  surf <- SDLi.load path'
  let (w, h) = (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf)
  let (tileW, tileH) = (floor $ w / xTiles, floor $ h / yTiles)
  let oneDir = floor $ tileW * tileH / 4
  let nOffset = oneDir * fromEnum direc
  let n = nOffset + floor $ fromIntegral oneDir * i / fps
  let (x, y) = (n `rem` xTiles, floor $ n / xTiles)
  let rect = SDL.Rect (x * tileW) (y * tileH) tileW tileH
  return (surf, rect)

animation :: Int -> Int -> Int -> String -> Word32 -> IO SurfPart
animation xTiles yTiles frameDur path = walkcycle xTiles (4 * yTiles) frameDur path $ toEnum 0

still :: String -> Word32 -> IO SurfPart
still path _ = (surf, SDL.getSize surf)
  where surf = SDLi.load path
