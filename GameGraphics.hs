module GameGraphics where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import Data.Ratio
import qualified Data.Map as Map
import qualified Control.Monad as CM
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

drawObject :: SDL.Surface -> Map.Map Position Position -> Int -> (Position, Cell) -> IO ()
drawObject screenSurf posChanges offset (p, obj) = do
--  print (p, x, y)
  let dr = Just $ SDL.Rect x y 0 0
  surf <- createColorSurf 64 64 (if isUser obj then 0xff0000ff else 0x00ff00ff)
  SDL.blitSurface surf Nothing screenSurf dr
  print offset
  return ()
  where
    (x, y) = maybe (p * (64, 64)) calcP $ Map.lookup p posChanges
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
            act $ 7 * (i + 1)
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

  gameLoop screenSurf background (game, Map.empty)
--  mapM_ SDL.freeSurface objects
  SDL.quit
  
  where 
    gameLoop screenSurf background (game@(gameMap, _), posChanges) = do
      animate 360 9 $ drawObjects posChanges
      drawObjects Map.empty 0
      eventLoop
      where 
        drawObjects posChanges n = do
          SDL.blitSurface background Nothing screenSurf Nothing
          mapM_ (drawObject screenSurf posChanges n) $ Map.toList gameMap
          SDL.flip screenSurf

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
