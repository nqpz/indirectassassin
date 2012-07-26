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
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Surfaces
import IndirectAssassin.Map
import IndirectAssassin.Logic

createSurf :: Int -> Int -> IO SDL.Surface
createSurf w h = SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff

fillSurf :: Word32 -> SDL.Surface -> IO ()
fillSurf color surf = SDL.fillRect surf Nothing $ SDL.Pixel color


theStartTime :: IORef Word32
theStartTime = unsafePerformIO $ newIORef 0

theFrameCount :: IORef Word32
theFrameCount = unsafePerformIO $ newIORef 0

theMaxFPS :: IORef Word32
theMaxFPS = unsafePerformIO $ newIORef 50

startCount :: IO ()
startCount = writeIORef theStartTime SDL.getTicks >> writeIORef theFrameCount 0

getTimePassed :: IO Word32
getTimePassed = do
  start <- readIORef theStartTime
  now <- SDL.getTicks
  return $ now - start

getFrameNumber :: IO Word32
getFrameNumber = readIORef theFrameCount

increaseFrameNumber :: IO ()
increaseFrameNumber = writeIORef theFrameCount $ 1 + readIORef theFrameCount

setMaxFPS :: Word32 -> IO ()
setMaxFPS fps = writeIORef theMaxFPS fps

getMaxFPS :: IO Word32
getMaxFPS = readIORef theMaxFPS

calculateFPS :: IO Double
calculateFPS = do
  time <- getTimePassed
  n <- getFrameNumber
  return fromIntegral n / 1000 * fromIntegral time

newtype GameExtra = GameExtra { getGame :: Game
                              , isCheating :: Bool
                              }

runGames :: [Game] -> IO ()
runGames games = do
  let (width, height) = (768, 576) -- hardcoded because I'm lazy (like the language, but different)

  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\game -> createInfCenterList [GameExtra (game, False)]) games
  let (gameListLists, (currentGameList, currentGame)) = createInfCenterList gameLists
  startCount
  gamesLoop screenSurf (gameListLists, (currentGameList, currentGame)) $ \() -> render screenSurf currentGame
  SDL.quit


gamesLoop :: SDL.Surface -> (CenterList (CenterList GameExtra, GameExtra), (CenterList GameExtra, GameExtra)) (() -> IO ()) -> IO ()
gamesLoop rootSurf all@(gameListLists, (currentGameList, currentGame)) runWhenNoEvents = do
  event <- SDL.pollEvent
  case event of 
    NoEvent -> runWhenNoEvents ()
    x       -> actOnEvent x
  where actOnEvent event = case eventAction x of 
          Nothing -> gamesLoop rootSurf all runWhenNoEvents
          Just t -> case t of
            PreviousGame -> gamesLoop rootSurf (gameListLists, previousGame) $ \() -> render rootSurf $ snd previousGame
            NextGame -> gamesLoop rootSurf (gameListLists, nextGame) $ \() -> render rootSurf $ snd nextGame
            PreviousMap -> gamesLoop rootSurf previousMap $ \() -> render rootSurf $ snd $ snd previousMap
            NextMap -> gamesLoop rootSurf nextMap $ \() -> render rootSurf $ snd $ snd nextMap
            ToggleCheat -> let newGameExtra = GameExtra (getGame currentGame, not $ isCheating currentGame)
                           in gamesLoop rootSurf (gameListLists, (currentGameList, newGameExtra)) $ \() -> render rootSurf newGameExtra
            AgentAction action -> gamesLoop rootSurf all makeAndShowNewGame
              where makeAndShowNewGame () = do
                      let newGame = GameExtra ((getGame currentGame) `step` action, isCheating currentGame)
                      renderInterpolated currentGame newGame
                      waitForNextFrame
                      gamesLoop rootSurf (gameListLists, (currentGameList, newGame)) \() -> waitForNextFrame
            Redraw -> gamesLoop rootSurf all \() -> render rootSurf currentGame
            ExitGame -> return ()
  where  
    previousGame = previousElement currentGameList currentGame
    nextGame     = nextElement     currentGameList currentGame
    previousMap  = previousElement gameListLists (currentGameList, currentGame)
    nextMap      = nextElement     gameListLists (currentGameList, currentGame)


waitForNextFrame :: IO ()
waitForNextFrame = do
  delay <- calculateDelay
  threadDelay $ floor delay
  increaseFrameNumber

calculateDelay :: IO Double
calculateDelay = do
  msecs <- getTimePassed
  n <- getFrameNumber
  fps <- getMaxFPS
  let delay = max 0 $ fromIntegral msecs - n * 1000 / fps
  return toIntegral $ delay * 1000

-- TODO: stop always cheating
render :: SDL.Surface -> GameExtra -> IO ()
render rootSurf gameExtra = do
  drawFloor game
  drawWall game
  drawItems game
  drawProfessors game
  drawAgent game
    where (game, cheat) = (getGame gameExtra, isCheating gameExtra)

-- TODO
renderInterPolated :: SDL.Surface -> GameExtra -> GameExtra -> IO ()
renderInterPolated rootSurf oldGame newGame = render rootSurf newGame

eventAction :: SDL.Event -> Maybe UserAction
eventAction (SDL.KeyDown (Keysym k mods c))
  | KeyModCtrl `elem` mods = case k of 
      SDLK_UP    -> Just PreviousGame
      SDLK_DOWN  -> Just NextGame
      SDLK_LEFT  -> Just PreviousMap
      SDLK_RIGHT -> Just NextMap
      SDLK_x     -> Just ToggleCheat
      SDLK_r     -> Just Redraw
  | k == SDLK_UP    = Just $ AgentAction $ Go Up
  | k == SDLK_LEFT  = Just $ AgentAction $ Go Left
  | k == SDLK_DOWN  = Just $ AgentAction $ Go Down
  | k == SDLK_RIGHT = Just $ AgentAction $ Go Right
  | k == SDLK_ESC   = Just ExitGame
  | k == SDLK_RETURN || k == SDLK_KP_ENTER = Accept
  | otherwise = maybe Nothing (Just . AgentAction . UseItem) $ charToItem $ toLower c
eventAction SDL.Quit = Just ExitGame


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
