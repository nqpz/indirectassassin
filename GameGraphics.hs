module GameGraphics where

-- Global
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.Color as SDLc
-- Local
import GameMap
import Logic

createColorSurf :: Word32 -> IO SDL.Surface
createColorSurf color = do
  surf <- SDL.createRGBSurface [] 64 74 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
  SDL.fillRect surf Nothing (SDLc.Pixel color)
  return surf

drawObject :: SDL.Surface -> (Position, Cell) -> IO ()
drawObject screenSurf ((x, y), obj) = do
  let dr = Just (SDL.Rect 0 0 0 0)
  surf <- createColorSurf 0xff0000ff
  SDL.blitSurface surf Nothing screenSurf dr
  return ()

runGame :: [IO Game] -> IO ()
runGame games = do
  game <- head games

  SDL.init [SDL.InitEverything]
  SDL.setVideoMode 640 480 32 []
  SDL.setCaption "Staelth Nijna" "staelth nijna"

  screenSurf <- SDL.getVideoSurface

  gameLoop screenSurf game
--  mapM_ SDL.freeSurface objects
  SDL.quit
  
  where 
    gameLoop screenSurf game@(gameMap, userPos) = do
      mapM_ (drawObject screenSurf) $ Map.toList gameMap
      SDL.flip screenSurf
      eventLoop
      where 
        checkEvent (SDL.KeyUp _) = return $ Just $ moveUser game Up
        checkEvent _             = do
          event <- SDL.waitEventBlocking
          checkEvent event
    
        eventLoop = do
          event <- SDL.waitEventBlocking
          print event
          newGame <- checkEvent event
          print 99
          print newGame
          maybe (return ()) (gameLoop screenSurf . nextStep) $ newGame
    
