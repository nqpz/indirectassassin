 module IndirectAssassin.GameRunner where

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
import IndirectAssassin.Misc
import IndirectAssassin.Surfaces
import IndirectAssassin.Map
import IndirectAssassin.Logic

createSurf :: Int -> Int -> IO SDL.Surface
createSurf w h = SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff

colorSurf :: Word32 -> SDL.Surface -> IO ()
colorSurf color surf = SDL.fillRect surf Nothing $ SDL.Pixel color

drawObject :: SDL.Surface -> Map.Map Position Position -> Int -> (Position, Cell) -> IO ()
drawObject screenSurf posChanges frameIdx (p, obj) = do
  let (surf, sr) = getSurf obj i
  let dr = SDL.Rect x y 0 0
  surf' <- surf
  SDL.blitSurface surf' (Just sr) screenSurf (Just dr)
  return ()
  where
    offset = 7 * (frameIdx + 1)
    ((x, y), i) = maybe (p * fromInteger 64, 0) (\t -> (calcP t, frameIdx)) $ Map.lookup p posChanges
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

runGames :: [Game] -> IO ()
runGames games = do
  let (width, height) = (768, 576) -- hardcoded because I'm lazy (like the language, but different)

  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\game -> createInfCenterList [game]) games
  let (gameListLists, currentGameList) = createInfCenterList vertical
  gamesLoop screenSurf gameListLists currentGameList
  SDL.quit

data Action = NoAction | PreviousGame | NextGame | PreviousMap | NextMap 
            | ToggleCheat | GoUp | GoLeft | GoDown | GoRight | Accept
            | Item Item

gamesLoop :: SDL.Surface -> CenterList (CenterList Game, Game) -> (CenterList Game, Game) -> IO ()
gamesLoop screenSurf gameListLists (currentGameList, currentGame) = do
  nextElement currentGameList currentGame
  nextElement gameListLists (currentGameList, currentGame)
  
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



eventAction :: SDL.Event -> Action
eventAction (SDL.KeyDown (Keysym k mods c))
  | KeyModCtrl `elem` mods = case k of 
      SDLK_UP    -> PreviousGame
      SDLK_DOWN  -> NextGame
      SDLK_LEFT  -> PreviousMap
      SDLK_RIGHT -> NextMap
      SDLK_x     -> ToggleCheat
  | k == SDLK_UP    = GoUp
  | k == SDLK_LEFT  = GoLeft
  | k == SDLK_DOWN  = GoDown
  | k == SDLK_RIGHT = GoRight
  | k == SDLK_RETURN || k == SDLK_KP_ENTER = Accept
  | otherwise = maybe NoAction Item $ charToItem $ toLower c 
