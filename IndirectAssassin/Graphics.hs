module IndirectAssassin.Graphics where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import Control.Monad
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLttf
import Paths_IndirectAssassin (getDataFileName)
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.BaseTypes
import IndirectAssassin.Map


(width, height) = (768, 576) -- hardcoded because I'm lazy (like the language, but different)


createSurf :: Int -> Int -> IO SDL.Surface
createSurf w h = SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff

fillSurf :: Word32 -> SDL.Surface -> IO Bool
fillSurf color surf = SDL.fillRect surf Nothing $ SDL.Pixel color


data Graphics = Graphics { getFloor :: SDL.Surface
                         , getWall :: SDL.Surface
                         , getFont :: SDLttf.Font
                         , getAgent :: Direction -> Word32 -> Word32 -> SurfPart
                         , getProfessor :: Direction -> Word32 -> Word32 -> SurfPart
                         , getSoldierNormal :: Direction -> Word32 -> Word32 -> SurfPart
                         , getSoldierZombie :: Direction -> Word32 -> Word32 -> SurfPart
                         , getBarrels :: Word32 -> Word32 -> SurfPart
                         , getBuckets :: Word32 -> Word32 -> SurfPart
                         , getBat :: Word32 -> Word32 -> SurfPart
                         , getBee :: Word32 -> Word32 -> SurfPart
                         , getDiamond :: Word32 -> Word32 -> SurfPart
                         , getTomato :: Word32 -> Word32 -> SurfPart
                         , getIceShield :: Word32 -> Word32 -> SurfPart
                         }

getGraphics :: IO Graphics
getGraphics = do
  floorSurf <- prepStill "data/floor.png"
  fullFloor <- createSurf width height
  fillSurf 0x000000ff fullFloor
  drawFloor floorSurf fullFloor
  wallSurf <- prepStill "data/wall.png"
  
  fontPath <- getDataFileName "data/embosst1.ttf"
  font <- SDLttf.openFont fontPath 20
  
  agentCycle <- prepWalkcycle 9 4 50 "data/character/agent.png"
  professorCycle <- prepWalkcycle 9 4 50 "data/character/professor.png"
  soldierNormalCycle <- prepWalkcycle 9 4 50 "data/character/soldier_normal.png"
  soldierZombieCycle <- prepWalkcycle 9 4 50 "data/character/soldier_zombie.png"
  
  barrels <- prepStill "data/item/barrels.png"
  buckets <- prepStill "data/item/buckets.png"
  bat <- prepAni 3 4 80 "data/item/bat_yellow.png"
  bee <- prepAni 3 4 80 "data/item/bee_green.png"
  diamond <- prepAni 4 1 120 "data/item/diamond.png"
  tomato <- prepAni 16 2 40 "data/item/tomato.png"
  iceShield <- prepAni 4 4 60 "data/item/ice_shield.png"
  
  return $ Graphics fullFloor wallSurf font
    (walkcycle agentCycle) (walkcycle professorCycle)
    (walkcycle soldierNormalCycle) (walkcycle soldierZombieCycle)
    (stillAni barrels) (stillAni buckets) (animation bat) (animation bee)
    (animation diamond) (animation tomato) (animation iceShield)
    

drawFloor :: SDL.Surface -> SDL.Surface -> IO [Bool]
drawFloor floorSurf destSurf = outM [ blitFloor (x, y) | x <- [0..ceiling $ fromIntegral width / 96], y <- [0..ceiling $ fromIntegral height / 32] ]
  where blitFloor :: Position -> IO Bool
        blitFloor (x, y) = do
          SDL.blitSurface floorSurf Nothing destSurf
            $ Just $ SDL.Rect (x * 96) (y * 32) 96 32

prepStill :: String -> IO SDL.Surface
prepStill path = SDLi.load =<< getDataFileName path

stillAni :: SDL.Surface -> Word32 -> Word32 -> SurfPart
stillAni surf _ _ = (surf, SDL.Rect 0 0 (SDL.surfaceGetWidth surf) (SDL.surfaceGetHeight surf))


type AnimationInfo = (SDL.Surface, Int, Int, Int, Int, Int)
animation :: AnimationInfo -> Word32 -> Word32 -> SurfPart
animation (surf, tileW, tileH, oneDir, xTiles, yTiles) i fps = (surf, rect)
  where n = floor $ fromIntegral oneDir * fromIntegral i / fromIntegral fps
        (x, y) = (n `rem` xTiles, floor $ fromIntegral n / fromIntegral xTiles)
        rect = SDL.Rect (x * tileW) (y * tileH) tileW tileH

prepAni :: Int -> Int -> Int -> String -> IO AnimationInfo
prepAni xTiles yTiles frameDur path = do
  path' <- getDataFileName path
  surf <- SDLi.load path'
  let (w, h) = (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf)
  let (tileW, tileH) = (floor $ fromIntegral w / fromIntegral xTiles, floor $ fromIntegral h / fromIntegral yTiles)
  let oneDir = floor $ fromIntegral xTiles * fromIntegral yTiles
  return (surf, tileW, tileH, oneDir, xTiles, yTiles)


walkcycle :: AnimationInfo -> Direction -> Word32 -> Word32 -> SurfPart
walkcycle (surf, tileW, tileH, oneDir, xTiles, yTiles) direc i fps = (surf, rect)
  where nOffset = oneDir * fromEnum direc
        n = nOffset + (floor $ fromIntegral oneDir * fromIntegral i / fromIntegral fps)
        (x, y) = (n `rem` xTiles, floor $ fromIntegral n / fromIntegral xTiles)
        rect = SDL.Rect (x * tileW) (y * tileH) tileW tileH

prepWalkcycle :: Int -> Int -> Int -> String -> IO AnimationInfo
prepWalkcycle xTiles yTiles frameDur path = do
          path' <- getDataFileName path
          surf <- SDLi.load path'
          let (w, h) = (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf)
          let (tileW, tileH) = (floor $ fromIntegral w / fromIntegral xTiles, floor $ fromIntegral h / fromIntegral yTiles)
          let oneDir = floor $ fromIntegral xTiles * fromIntegral yTiles / 4
          return (surf, tileW, tileH, oneDir, xTiles, yTiles)
