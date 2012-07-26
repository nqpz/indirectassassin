 module IndirectAssassin.GameRunner where

 -- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import Data.Ratio
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLttf
import Graphics.UI.SDL.Keysym
import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.BaseTypes
import IndirectAssassin.Graphics
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

theFPS :: IORef Word32
theFPS = unsafePerformIO $ newIORef 50

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

getFPS :: IO Word32
getFPS = readIORef theFPS

updateFPS :: IO ()
updateFPS = writeIORef theFPS $ floor calculateFPS

calculateFPS :: IO Double
calculateFPS = do
  time <- getTimePassed
  n <- getFrameNumber
  return fromIntegral n / 1000 * fromIntegral time


(width, height) = (768, 576) -- hardcoded because I'm lazy (like the language, but different)

runGames :: [Game] -> IO ()
runGames games = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\game -> createInfCenterList [GameExtra (game, Nothing, False, game)]) games
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
            ToggleCheat -> let newGameExtra = GameExtra (getGame currentGame, hasWon currentGame, not $ isCheating currentGame, getOrigGame currentGame)
                           in gamesLoop rootSurf (gameListLists, (currentGameList, newGameExtra)) $ \() -> render rootSurf newGameExtra
            Accept -> maybe (gamesLoop rootSurf all runWhenNoEvents) 
                      (const (gamesLoop rootSurf (gameListLists, (currentGameList, newGameExtra)) 
                              $ \() -> render rootSurf newGameExtra)) (hasWon currentGame)
                      where newGameExtra = GameExtra (getOrigGame currentGame, Nothing, isCheating currentGame, getOrigGame currentGame)
            AgentAction action -> gamesLoop rootSurf all makeAndShowNewGame
              where makeAndShowNewGame () = do
                      case hasWon currentGame of
                        Nothing -> makeAndShow'
                        Just b -> renderEndScreen b
                    makeAndShow' = do
                      let (stepEffect, game') = (getGame currentGame) `step` action
                      let newGame = GameExtra (game', case stepEffect of 
                                                  GameWon b -> Just b
                                                  _ -> Nothing, isCheating currentGame,
                                               getOrigGame currentGame)
                      case stepEffect of
                        NoChange -> return ()
                        NewGame -> renderInterpolated rootSurf currentGame newGame
                        GameWon b -> renderInterpolated rootSurf currentGame newGame >> renderEndScreen rootSurf b

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
  updateFPS

calculateDelay :: IO Double
calculateDelay = do
  msecs <- getTimePassed
  n <- getFrameNumber
  fps <- getMaxFPS
  let delay = max 0 $ fromIntegral msecs - n * 1000 / fps
  return toIntegral $ delay * 1000

drawFloor :: SDL.Surface -> IO ()
drawFloor surf = [ blitFloor x y | x <- [0..ceiling $ width / 96], y <- [0..ceiling $ height / 32] ]
  where blitFloor x y = do
          floorSurf <- floorS
          return $ SDL.blitSurface floorSurf Nothing surf
            $ Just $ Rect (x * 96) (y * 32) 96 32

drawItems :: SDL.Surface -> Game -> IO ()
drawItems surf game = [ blitItem pos item | (pos, Item item) <- filter isItem $ Map.toList game ]
  where blitItem (x, y) item = do
          (itemSurf, itemRect) <- itemToImage item getFrameNumber getFPS
          let offset = (floor $ (64 - rectW itemRect) / 2, floor $ (64 - rectH itemRect) / 2)
          return $ SDL.blitSurface itemSurf itemRect surf
            $ Just $ Rect (x * 64 + fst offset) (y * 64 + snd offset) (rectW itemRect) (rectH itemRect)
  
drawProfessors :: SDL.Surface -> Game -> IO ()
drawProfessors surf game = [ blitProf dir pos items | (pos, Professor dir items) <- filter isProfessor $ Map.toList game ]
  where blitProf dir pos@(x, y) items = do
          (profSurf, profRect) <- profSprite dir pos items getFrameNumber getFPS
          return $ SDL.blitSurface profSurf profRect surf
            $ Just $ Rect (x * 64) (y * 64) 64 64
        
drawAgent :: SDL.Surface -> Game -> IO ()
drawAgent surf game = do
  let ((x, y), Agent dir items) = head $ filter isAgent $ Map.toList game
  (agentSurf, agentRect) <- agent getFrameNumber getFPS
  return $ SDL.blitSurface agentSurf agentRect surf
    $ Just $ Rect (x * 64) (y * 64) 64 64

drawWalls :: SDL.Surface -> Game -> IO ()
drawWalls surf game = [ blitWall pos | pos <- sortBy depth $ map fst $ filter isWall $ Map.toList game ]
  where depth (_, y0) (_, y1) = compare y0 y1
        blitWall (x, y) = do
          wallSurf <- wall 
          return $ SDL.blitSurface wallSurf Nothing surf
            $ Just $ Rect (x * 64) (y * 64 - 26) 64 88

drawDarkness :: SDL.Surface -> Game -> IO ()
drawDarkness = do
  return ()

render :: SDL.Surface -> GameExtra -> IO ()
render rootSurf gameExtra = do
  drawFloor rootSurf
  drawItems rootSurf game
  drawProfessors rootSurf game
  drawAgent rootSurf game
  drawWalls rootSurf game
  if cheat then return () else drawDarkness game
  waitForNextFrame
    where (game, cheat) = (getGame gameExtra, isCheating gameExtra)

-- TODO
renderInterPolated :: SDL.Surface -> GameExtra -> GameExtra -> IO ()
renderInterPolated rootSurf oldGame newGame = render rootSurf newGame

renderEndScreen :: SDL.Surface -> Bool -> IO ()
renderEndScreen rootSurf won = do
  fillSurf (if won then 0x0000ffff else 0xff0000ff) rootSurf
  SDLttf.renderTextSolid font $ message won $ SDL.Color 0 0 0
  where message True  = "You have won!"
        message False = "You have lost!"
  waitForNextFrame

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
