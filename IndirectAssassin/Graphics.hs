{--
Indirect Assassin: a turn-based stealth game
Copyright (C) 2012  Niels G. W. Serup

This file is part of Indirect Assassin.

Indirect Assassin is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Indirect Assassin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
Indirect Assassin.  If not, see <http://www.gnu.org/licenses/>.
--}

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
import qualified Graphics.UI.SDL.Mixer as SDLmix
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
                         , getAgent :: Direction -> (Word32 -> Word32 -> SurfPart, SurfPart)
                         , getProfessor :: Direction -> (Word32 -> Word32 -> SurfPart, SurfPart)
                         , getSoldierNormal :: Direction -> (Word32 -> Word32 -> SurfPart, SurfPart)
                         , getSoldierZombie :: Direction -> (Word32 -> Word32 -> SurfPart, SurfPart)
                         , getBarrels :: Word32 -> Word32 -> SurfPart
                         , getBuckets :: Word32 -> Word32 -> SurfPart
                         , getBat :: Word32 -> Word32 -> SurfPart
                         , getBee :: Word32 -> Word32 -> SurfPart
                         , getDiamond :: Word32 -> Word32 -> SurfPart
                         , getTomato :: Word32 -> Word32 -> SurfPart
                         , getIceShield :: Word32 -> Word32 -> SurfPart
                         , getToilet :: Word32 -> Word32 -> SurfPart
                         , getLightingSurf :: Lighting -> SDL.Surface
                         -- , getBackgroundMusic :: SDLmix.Music
                         }

getGraphics :: IO Graphics
getGraphics = do
  floorSurf <- prepStill "data/floor.png"
  fullFloor <- createSurf (width + 192) height
  fillSurf 0x000000ff fullFloor
  drawFloor floorSurf fullFloor
  SDL.freeSurface floorSurf
  wallSurf <- prepStill "data/wall.png"
  
  fontPath <- getDataFileName "data/embosst1.ttf"
  font <- SDLttf.openFont fontPath 30
  
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
  toilet <- prepStill "data/item/toilet.png"
  
  darknessSurf <- createSurf 64 64
  flashlightSurf <- createSurf 64 64
  nightVisionSurf <- createSurf 64 64
  fillSurf 0x000000ff darknessSurf
  fillSurf 0xfff22477 flashlightSurf
  fillSurf 0x0000ff77 nightVisionSurf
  
  -- bgMusPath <- getDataFileName "data/sound/background_music.ogg"
  -- backgroundMusic <- SDLmix.loadMUS bgMusPath
  
  return $ Graphics fullFloor wallSurf font
    (dirExpand (walkcycle agentCycle, standStill agentCycle))
    (dirExpand (walkcycle professorCycle, standStill professorCycle))
    (dirExpand (walkcycle soldierNormalCycle, standStill soldierNormalCycle))
    (dirExpand (walkcycle soldierZombieCycle, standStill soldierZombieCycle))
    (stillAni barrels) (stillAni buckets) (animation bat) (animation bee)
    (animation diamond) (animation tomato) (animation iceShield)
    (stillAni toilet)
    (\l -> case l of Darkness -> darknessSurf
                     Flashlight -> flashlightSurf
                     NightVision -> nightVisionSurf)
    -- backgroundMusic
  where dirExpand (ani, still) direc = (ani direc, still direc)
        
itemToImage :: Graphics -> Item -> Word32 -> Word32 -> SurfPart
itemToImage g i = (\f -> f g) $ case i of
  Barrels   -> getBarrels
  Buckets   -> getBuckets
  YellowBat -> getBat
  GreenBee  -> getBee
  Diamond   -> getDiamond
  Tomato    -> getTomato
  IceShield -> getIceShield
  Toilet    -> getToilet
        
closeGraphics :: Graphics -> IO ()
closeGraphics graphics = do
  SDL.freeSurface $ getFloor graphics
  SDL.freeSurface $ getWall graphics
  SDLttf.closeFont $ getFont graphics
  freeCycle getAgent
  freeCycle getProfessor
  freeCycle getSoldierNormal
  freeCycle getSoldierZombie
  freeAni getBarrels
  freeAni getBuckets
  freeAni getBat
  freeAni getBee
  freeAni getDiamond
  freeAni getTomato
  freeAni getIceShield
  -- SDLmix.freeMusic $ getBackgroundMusic graphics
  where freeCycle f = SDL.freeSurface $ fst $ snd $ f graphics Up
        freeAni f   = SDL.freeSurface $ fst $ f graphics 0 1

drawFloor :: SDL.Surface -> SDL.Surface -> IO [Bool]
drawFloor floorSurf destSurf = outM [ blitFloor (x, y) | x <- [0..ceiling $ fromIntegral width / 96 + fromIntegral 2], y <- [0..ceiling $ fromIntegral height / 32] ]
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
        n = nOffset + (floor $ fromIntegral (oneDir - 1) * fromIntegral i / fromIntegral fps) + 1
        (x, y) = (n `rem` (xTiles - 1), floor $ fromIntegral n / fromIntegral xTiles)
        rect = SDL.Rect (x * tileW) (y * tileH) tileW tileH

prepWalkcycle :: Int -> Int -> Int -> String -> IO AnimationInfo
prepWalkcycle xTiles yTiles frameDur path = do
          path' <- getDataFileName path
          surf <- SDLi.load path'
          let (w, h) = (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf)
          let (tileW, tileH) = (floor $ fromIntegral w / fromIntegral xTiles, floor $ fromIntegral h / fromIntegral yTiles)
          let oneDir = floor $ fromIntegral xTiles * fromIntegral yTiles / 4
          return (surf, tileW, tileH, oneDir, xTiles, yTiles)

standStill :: AnimationInfo -> Direction -> SurfPart
standStill (surf, _, _, _, _, _) direc = (surf, SDL.Rect 0 (64 * fromEnum direc) 64 64)
