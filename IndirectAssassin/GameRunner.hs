 module IndirectAssassin.GameRunner where

 -- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import Data.Word
import Data.Char
import Data.List (foldl', sortBy)
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


theStartTime :: IORef Word32
theStartTime = unsafePerformIO $ newIORef 0

theFrameCount :: IORef Word32
theFrameCount = unsafePerformIO $ newIORef 0

theMaxFPS :: IORef Word32
theMaxFPS = unsafePerformIO $ newIORef 50

theFPS :: IORef Word32
theFPS = unsafePerformIO $ newIORef 50

startCount :: IO ()
startCount = (writeIORef theStartTime =<< SDL.getTicks) >> writeIORef theFrameCount 0

getTimePassed :: IO Word32
getTimePassed = do
  start <- readIORef theStartTime
  now <- SDL.getTicks
  return $ now - start

getFrameNumber :: IO Word32
getFrameNumber = readIORef theFrameCount

increaseFrameNumber :: IO ()
increaseFrameNumber = do
  fc <- readIORef theFrameCount
  writeIORef theFrameCount (1 + fc)

setMaxFPS :: Word32 -> IO ()
setMaxFPS fps = writeIORef theMaxFPS fps

getMaxFPS :: IO Word32
getMaxFPS = readIORef theMaxFPS

getFPS :: IO Word32
getFPS = readIORef theFPS

updateFPS :: IO ()
updateFPS = do
  time <- getTimePassed
  fn <- getFrameNumber
  writeIORef theFPS $ floor $ calculateFPS time fn

calculateFPS :: Word32 -> Word32 -> Double
calculateFPS time fn = fromIntegral fn * 1000 / fromIntegral time


runGames :: [Game] -> IO ()
runGames games = do
  SDL.init [SDL.InitEverything]
  SDLttf.init
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  graphics <- getGraphics
  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\game -> createInfCenterList [GameExtra game Nothing False game]) games
  let (gameListLists, (currentGameList, currentGame)) = createInfCenterList gameLists
  startCount
  putStrLn "Start."
  gamesLoop screenSurf graphics (gameListLists, (currentGameList, currentGame))
  putStrLn "End."
  SDL.quit
  SDLttf.quit


gamesLoop :: SDL.Surface -> Graphics -> (CenterList (CenterList GameExtra, GameExtra), (CenterList GameExtra, GameExtra)) -> IO ()
gamesLoop rootSurf graphics all@(gameListLists, (currentGameList, currentGame)) = do
  event <- SDL.pollEvent
  case event of 
    SDL.NoEvent -> render rootSurf graphics currentGame >> gamesLoop rootSurf graphics all
    x           -> actOnEvent x
  where actOnEvent event = case eventAction event of
          Nothing -> render rootSurf graphics currentGame >> gamesLoop rootSurf graphics all
          Just t -> case t of
            PrevGame -> render rootSurf graphics (snd prevGame) >> gamesLoop rootSurf graphics (gameListLists, prevGame)
            NextGame -> render rootSurf graphics (snd nextGame) >> gamesLoop rootSurf graphics (gameListLists, nextGame)
            PrevMap -> render rootSurf graphics (snd $ snd prevMap) >> gamesLoop rootSurf graphics prevMap
            NextMap -> render rootSurf graphics (snd $ snd prevMap) >> gamesLoop rootSurf graphics nextMap
            ToggleCheat -> let newGameExtra = GameExtra (getGame currentGame) (hasWon currentGame) (not $ isCheating currentGame) (getOrigGame currentGame)
                           in render rootSurf graphics newGameExtra >> gamesLoop rootSurf graphics (gameListLists, (currentGameList, newGameExtra))
            Accept -> maybe (render rootSurf graphics currentGame >> gamesLoop rootSurf graphics all) 
                      (const (render rootSurf graphics newGameExtra >> gamesLoop rootSurf graphics (gameListLists, (currentGameList, newGameExtra)))) (hasWon currentGame)
                      where newGameExtra = GameExtra (getOrigGame currentGame) Nothing (isCheating currentGame) (getOrigGame currentGame)
            AgentAction action -> makeAndShowNewGame
              where makeAndShowNewGame = do
                      case hasWon currentGame of
                        Nothing -> makeAndShow'
                        Just b -> renderEndScreen rootSurf graphics b
                    makeAndShow' = do
                      let (stepEffect, game') = (getGame currentGame) `step` action
                      let hasWon' = case stepEffect of
                            GameWon b -> Just b
                            _ -> Nothing
                      let newGame = GameExtra game' hasWon' (isCheating currentGame) (getOrigGame currentGame)
                      case stepEffect of
                        NoChange -> return ()
                        NewGame -> renderInterpolated rootSurf graphics currentGame newGame
                        GameWon b -> renderInterpolated rootSurf graphics currentGame newGame >> renderEndScreen rootSurf graphics b
                      gamesLoop rootSurf graphics (gameListLists, (currentGameList, newGame))
            Redraw -> render rootSurf graphics currentGame >> gamesLoop rootSurf graphics all
            ExitGame -> return ()
          where  
            prevGame = prevElement currentGameList currentGame
            nextGame = nextElement currentGameList currentGame
            prevMap  = prevElement gameListLists (currentGameList, currentGame)
            nextMap  = nextElement gameListLists (currentGameList, currentGame)


waitForNextFrame :: IO ()
waitForNextFrame = do
  delay <- calculateDelay
  threadDelay $ floor delay
  increaseFrameNumber
  updateFPS
  print =<< getFPS

calculateDelay :: IO Double
calculateDelay = do
  msecs <- getTimePassed
  fn <- getFrameNumber
  fps <- getMaxFPS
  let delay = max 0 $ 1000 * fromIntegral fn / fromIntegral fps - fromIntegral msecs
  return $ delay * 1000

itemToImage :: Graphics -> Item -> Word32 -> Word32 -> SurfPart
itemToImage g i = case i of
  Barrels -> getBarrels g
  Buckets -> getBuckets g
  YellowBat -> getBat g
  GreenBee -> getBee g
  Diamond -> getDiamond g
  Tomato -> getTomato g
  IceShield -> getIceShield g

drawItems :: SDL.Surface -> Graphics -> Game -> IO [Bool]
drawItems surf graphics game = outM [ blitItem pos item | (pos, Item item) <- filter (isItem . snd) $ Map.toList game ]
  where blitItem :: Position -> Item -> IO Bool
        blitItem (x, y) item = do
          fn <- getFrameNumber
          fps <- getFPS
          let (itemSurf, itemRect) = itemToImage graphics item (fn `rem` fps) fps
          let offset = (floor $ fromIntegral (64 - SDL.rectW itemRect) / 2, floor $ fromIntegral (64 - SDL.rectH itemRect) / 2)
          SDL.blitSurface itemSurf (Just itemRect) surf
            $ Just $ SDL.Rect (x * 64 + fst offset) (y * 64 + snd offset) (SDL.rectW itemRect) (SDL.rectH itemRect)
  
drawProfessors :: SDL.Surface -> Graphics -> Game -> IO [Bool]
drawProfessors surf graphics game = outM [ blitProf dir pos items | (pos, Professor dir items) <- filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          fn <- getFrameNumber
          fps <- getFPS
          let (profSurf, profRect) = profSprite graphics dir pos items (fn `rem` fps) fps
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (x * 64) (y * 64) 64 64
        
drawAgent :: SDL.Surface -> Graphics -> Game -> IO Bool
drawAgent surf graphics game = do
  let ((x, y), Agent dir items) = head $ filter (isAgent . snd) $ Map.toList game
  fn <- getFrameNumber
  fps <- getFPS
  let (agentSurf, agentRect) = (getAgent graphics) dir (fn `rem` fps) fps
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (x * 64) (y * 64) 64 64

drawWalls :: SDL.Surface -> Graphics -> Game -> IO [Bool]
drawWalls surf graphics game = outM [ blitWall pos | pos <- sortBy depth $ map fst $ filter (isWall . snd) $ Map.toList game ]
  where depth (_, y0) (_, y1) = compare y0 y1
        blitWall :: Position -> IO Bool
        blitWall (x, y) = do
          SDL.blitSurface (getWall graphics) Nothing surf
            $ Just $ SDL.Rect (x * 64) (y * 64 - 26) 64 88

drawDarkness :: SDL.Surface -> Graphics -> Game -> IO ()
drawDarkness surf graphics game = do
  return ()

render :: SDL.Surface -> Graphics -> GameExtra -> IO ()
render rootSurf graphics gameExtra = do
  case hasWon gameExtra of
    Nothing -> render'
    Just b -> renderEndScreen rootSurf graphics b
  where render' = do
          SDL.blitSurface (getFloor graphics) Nothing rootSurf Nothing
          drawItems rootSurf graphics game
          drawProfessors rootSurf graphics game
          drawAgent rootSurf graphics game
          drawWalls rootSurf graphics game
          if cheat then return () else drawDarkness rootSurf graphics game
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)

-- TODO
renderInterpolated :: SDL.Surface -> Graphics -> GameExtra -> GameExtra -> IO ()
renderInterpolated rootSurf graphics oldGame newGame = render rootSurf graphics newGame

renderEndScreen :: SDL.Surface -> Graphics -> Bool -> IO ()
renderEndScreen rootSurf graphics won = do
  fillSurf (if won then 0x0000ffff else 0xff0000ff) rootSurf
  SDLttf.renderTextSolid (getFont graphics) (message won) $ SDL.Color 0 0 0
  waitForNextFrame
  where message True  = "You have won!"
        message False = "You have lost!"

eventAction :: SDL.Event -> Maybe UserAction
eventAction (SDL.KeyDown (Keysym k mods c))
  | KeyModCtrl `elem` mods = case k of 
      SDLK_UP    -> Just PrevGame
      SDLK_DOWN  -> Just NextGame
      SDLK_LEFT  -> Just PrevMap
      SDLK_RIGHT -> Just NextMap
      SDLK_x     -> Just ToggleCheat
      SDLK_r     -> Just Redraw
  | k == SDLK_UP     = Just $ AgentAction $ Go Up
  | k == SDLK_LEFT   = Just $ AgentAction $ Go Left
  | k == SDLK_DOWN   = Just $ AgentAction $ Go Down
  | k == SDLK_RIGHT  = Just $ AgentAction $ Go Right
  | k == SDLK_ESCAPE = Just ExitGame
  | k == SDLK_RETURN || k == SDLK_KP_ENTER = Just Accept
  | otherwise = maybe Nothing (Just . AgentAction . UseItem) $ charToItem $ toLower c
eventAction SDL.Quit = Just ExitGame
eventAction _ = Nothing
