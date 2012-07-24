module GameGraphics where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import Data.Ratio
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.Keysym
import Control.Concurrent (threadDelay)
-- Local
import Misc
import GameMap
import Logic

createColorSurf :: Int -> Int -> Word32 -> IO SDL.Surface
createColorSurf w h color = do
  surf <- SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
  SDL.fillRect surf Nothing $ SDL.Pixel color
  return surf

dirToWCN Up    = 0
dirToWCN Left  = 1
dirToWCN Down  = 2
dirToWCN Right = 3

walkCycle d i surf = (surf, SDL.Rect (64 * i) (64 * (dirToWCN d)) 64 64)

itemAnimations = []
mkAnimation "Matthew_Nash__Public Toilet Tileset/toilet.png" 64 1
mkAnimation "base_assets/3barrels.png" 64 1
mkAnimation "base_assets/4buckets.png" 64 1
mkAnimation "base_assets/bat_yellow.png" 32 12
mkAnimation "base_assets/bee_green.png" 32 12

mkAnimation "base_assets/wall.png" 32 12


soldierNormal = SDLi.load "Barbara_Rivera__Concept Art for LPC Entry/malesoldiernormal.png"
soldierZombie = SDLi.load "Barbara_Rivera__Concept Art for LPC Entry/malesoldierzombie.png"
                            
fbi = SDLi.load "Skyler_Robert_Colladay__FeralFantom's Entry/FBI_walk_cycle.png"
professor = SDLi.load "Skyler_Robert_Colladay__FeralFantom's Entry/professor_walk_cycle_no_hat.png"

getSurf (Guard d _ _) i = walkCycle d i male
getSurf (User d _)    i = walkCycle d i male
getSurf Wall          _ = (createColorSurf 64 64 0x0000ffff, SDL.Rect 0 0 64 64)
getSurf (Item _)      _ = (createColorSurf 64 64 0x0000ffff, SDL.Rect 0 0 64 64)
getSurf Empty         _ = (createColorSurf 64 64 0x00000000, SDL.Rect 0 0 64 64)

drawObject :: SDL.Surface -> Map.Map Position Position -> Int -> (Position, Cell) -> IO ()
drawObject screenSurf posChanges frameIdx (p, obj) = do
  let (surf, sr) = getSurf obj i
  let dr = SDL.Rect x y 0 0
  surf' <- surf
  SDL.blitSurface surf' (Just sr) screenSurf (Just dr)
  return ()
  where
    offset = 7 * (frameIdx + 1)
    ((x, y), i) = maybe (p * (64, 64), 0) (\t -> (calcP t, frameIdx)) $ Map.lookup p posChanges
    calcP (x', y') = (x' * 64 + (calcOffset x' $ fst p), y' * 64 + (calcOffset y' $ snd p))
    calcOffset t' t | t' < t = offset
                    | t' > t = -offset
                    | otherwise = 0

animate :: Int -> Int -> (Int -> IO ()) -> IO ()
animate msecs nFrames act = do
  t <- SDL.getTicks
  anim t 0
  where anim t i = do
          if i == nFrames
            then return ()
            else do
            act i
            t' <- SDL.getTicks
            let delay = max (fromIntegral i * frmDur - 1000.0 * (fromIntegral (t' - t))) 0
            threadDelay $ floor delay
            anim t $ i + 1
        frmDur = 1000 * fromIntegral msecs / fromIntegral nFrames :: Double

runGame :: [Game] -> IO ()
runGame games = do
  let game = head games
  let (width, height) = (800, 600)

  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Staelth Nijna" "staelth nijna"

  screenSurf <- SDL.getVideoSurface
  background <- createColorSurf width height 0x000000ff

  drawObjects screenSurf background game Map.empty 0
  gameLoop screenSurf background (game, Map.empty)
--  mapM_ SDL.freeSurface objects
  SDL.quit
  
  where 
    drawObjects screenSurf background (gameMap, _) posChanges n = do
      SDL.blitSurface background Nothing screenSurf Nothing
      mapM_ (drawObject screenSurf posChanges n) $ Map.toList gameMap
      SDL.flip screenSurf
    
    gameLoop screenSurf background (game@(gameMap, _), posChanges) = do
      animate 360 9 $ drawObjects' posChanges
      drawObjects' Map.empty 0
      eventLoop
      where 
        drawObjects' = drawObjects screenSurf background game

        checkEvent (SDL.KeyDown (Keysym k _ _))
          | k == SDLK_UP    = r Up
          | k == SDLK_RIGHT = r Right
          | k == SDLK_DOWN  = r Down
          | k == SDLK_LEFT  = r Left
          where r d = return $ Just $ moveUser game d
        checkEvent SDL.Quit = return Nothing
        checkEvent _             = do
          event <- SDL.waitEventBlocking
          checkEvent event
    
        eventLoop = do
          event <- SDL.waitEventBlocking
          newGame <- checkEvent event
          let newGame2 = maybe Nothing doStuff newGame
          maybe (return ()) (gameLoop screenSurf background) $ newGame2
        doStuff (game, userPosChange) = maybe Nothing doStuff2 $ nextStep game
          where doStuff2 (game', otherPosChanges) = Just (game', Map.union userPosChange otherPosChanges)
