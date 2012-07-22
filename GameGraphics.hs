module GameGraphics where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Color as SDLc
import Graphics.UI.SDL.Keysym
-- Local
import GameMap
import Logic

createColorSurf :: Int -> Int -> Word32 -> IO SDL.Surface
createColorSurf w h color = do
  surf <- SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
  SDL.fillRect surf Nothing (SDLc.Pixel color)
  return surf

drawObject :: SDL.Surface -> (Position, Cell) -> IO ()
drawObject screenSurf ((x, y), obj) = do
  let dr = Just (SDL.Rect (x * 64) (y * 64) 0 0)
  print obj
  surf <- createColorSurf 64 64 (if isUser obj then 0xff0000ff else 0x00ff00ff)
  SDL.blitSurface surf Nothing screenSurf dr
  return ()

runGame :: [IO Game] -> IO ()
runGame games = do
  game <- head games
  let (width, height) = (800, 600)

  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Staelth Nijna" "staelth nijna"

  screenSurf <- SDL.getVideoSurface
  background <- createColorSurf width height 0x000000ff

  gameLoop screenSurf background game
--  mapM_ SDL.freeSurface objects
  SDL.quit
  
  where 
    gameLoop screenSurf background game@(gameMap, userPos) = do
      SDL.blitSurface background Nothing screenSurf Nothing      
      mapM_ (drawObject screenSurf) $ Map.toList gameMap
      SDL.flip screenSurf
      eventLoop
      where 
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
          maybe (return ()) (gameLoop screenSurf background . nextStep) $ newGame
    
