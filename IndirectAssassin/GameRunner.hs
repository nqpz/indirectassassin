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

createSurf :: Int -> Int -> IO SDL.Surface
createSurf w h = SDL.createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff

fillSurf :: Word32 -> SDL.Surface -> IO Bool
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


(width, height) = (768, 576) -- hardcoded because I'm lazy (like the language, but different)

runGames :: [Game] -> IO ()
runGames games = do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\game -> createInfCenterList [GameExtra game Nothing False game]) games
  let (gameListLists, (currentGameList, currentGame)) = createInfCenterList gameLists
  startCount
  putStrLn "Start."
  gamesLoop screenSurf (gameListLists, (currentGameList, currentGame))
  putStrLn "End."
  SDL.quit


gamesLoop :: SDL.Surface -> (CenterList (CenterList GameExtra, GameExtra), (CenterList GameExtra, GameExtra)) -> IO ()
gamesLoop rootSurf all@(gameListLists, (currentGameList, currentGame)) = do
  event <- SDL.pollEvent
  case event of 
    SDL.NoEvent -> render rootSurf currentGame >> gamesLoop rootSurf all
    x           -> actOnEvent x
  where actOnEvent event = case eventAction event of
          Nothing -> render rootSurf currentGame >> gamesLoop rootSurf all
          Just t -> case t of
            PrevGame -> render rootSurf (snd prevGame) >> gamesLoop rootSurf (gameListLists, prevGame)
            NextGame -> render rootSurf (snd nextGame) >> gamesLoop rootSurf (gameListLists, nextGame)
            PrevMap -> render rootSurf (snd $ snd prevMap) >> gamesLoop rootSurf prevMap
            NextMap -> render rootSurf (snd $ snd prevMap) >> gamesLoop rootSurf nextMap
            ToggleCheat -> let newGameExtra = GameExtra (getGame currentGame) (hasWon currentGame) (not $ isCheating currentGame) (getOrigGame currentGame)
                           in render rootSurf newGameExtra >> gamesLoop rootSurf (gameListLists, (currentGameList, newGameExtra))
            Accept -> maybe (render rootSurf currentGame >> gamesLoop rootSurf all) 
                      (const (render rootSurf newGameExtra >> gamesLoop rootSurf (gameListLists, (currentGameList, newGameExtra)))) (hasWon currentGame)
                      where newGameExtra = GameExtra (getOrigGame currentGame) Nothing (isCheating currentGame) (getOrigGame currentGame)
            AgentAction action -> makeAndShowNewGame
              where makeAndShowNewGame = do
                      case hasWon currentGame of
                        Nothing -> makeAndShow'
                        Just b -> renderEndScreen rootSurf b
                    makeAndShow' = do
                      let (stepEffect, game') = (getGame currentGame) `step` action
                      let hasWon' = case stepEffect of
                            GameWon b -> Just b
                            _ -> Nothing
                      let newGame = GameExtra game' hasWon' (isCheating currentGame) (getOrigGame currentGame)
                      case stepEffect of
                        NoChange -> return ()
                        NewGame -> renderInterpolated rootSurf currentGame newGame
                        GameWon b -> renderInterpolated rootSurf currentGame newGame >> renderEndScreen rootSurf b
                      gamesLoop rootSurf (gameListLists, (currentGameList, newGame))
            Redraw -> render rootSurf currentGame >> gamesLoop rootSurf all
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

drawFloor :: SDL.Surface -> IO [Bool]
drawFloor surf = outM [ blitFloor (x, y) | x <- [0..ceiling $ fromIntegral width / 96], y <- [0..ceiling $ fromIntegral height / 32] ]
  where blitFloor :: Position -> IO Bool
        blitFloor (x, y) = do
          floorSurf <- floorS
          SDL.blitSurface floorSurf Nothing surf
            $ Just $ SDL.Rect (x * 96) (y * 32) 96 32

drawItems :: SDL.Surface -> Game -> IO [Bool]
drawItems surf game = outM [ blitItem pos item | (pos, Item item) <- filter (isItem . snd) $ Map.toList game ]
  where blitItem :: Position -> Item -> IO Bool
        blitItem (x, y) item = do
          fn <- getFrameNumber
          fps <- getFPS
          (itemSurf, itemRect) <- itemToImage item (fn `rem` fps) fps
          let offset = (floor $ fromIntegral (64 - SDL.rectW itemRect) / 2, floor $ fromIntegral (64 - SDL.rectH itemRect) / 2)
          SDL.blitSurface itemSurf (Just itemRect) surf
            $ Just $ SDL.Rect (x * 64 + fst offset) (y * 64 + snd offset) (SDL.rectW itemRect) (SDL.rectH itemRect)
  
drawProfessors :: SDL.Surface -> Game -> IO [Bool]
drawProfessors surf game = outM [ blitProf dir pos items | (pos, Professor dir items) <- filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          fn <- getFrameNumber
          fps <- getFPS
          (profSurf, profRect) <- profSprite dir pos items (fn `rem` fps) fps
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (x * 64) (y * 64) 64 64
        
drawAgent :: SDL.Surface -> Game -> IO Bool
drawAgent surf game = do
  let ((x, y), Agent dir items) = head $ filter (isAgent . snd) $ Map.toList game
  fn <- getFrameNumber
  fps <- getFPS
  (agentSurf, agentRect) <- agent dir (fn `rem` fps) fps
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (x * 64) (y * 64) 64 64

drawWalls :: SDL.Surface -> Game -> IO [Bool]
drawWalls surf game = outM [ blitWall pos | pos <- sortBy depth $ map fst $ filter (isWall . snd) $ Map.toList game ]
  where depth (_, y0) (_, y1) = compare y0 y1
        blitWall :: Position -> IO Bool
        blitWall (x, y) = do
          wallSurf <- wall 
          SDL.blitSurface wallSurf Nothing surf
            $ Just $ SDL.Rect (x * 64) (y * 64 - 26) 64 88

drawDarkness :: SDL.Surface -> Game -> IO ()
drawDarkness surf game = do
  return ()

render :: SDL.Surface -> GameExtra -> IO ()
render rootSurf gameExtra = do
  case hasWon gameExtra of
    Nothing -> render'
    Just b -> renderEndScreen rootSurf b
  where render' = do
          drawFloor rootSurf
          drawItems rootSurf game
          drawProfessors rootSurf game
          drawAgent rootSurf game
          drawWalls rootSurf game
          if cheat then return () else drawDarkness rootSurf game
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)

-- TODO
renderInterpolated :: SDL.Surface -> GameExtra -> GameExtra -> IO ()
renderInterpolated rootSurf oldGame newGame = render rootSurf newGame

renderEndScreen :: SDL.Surface -> Bool -> IO ()
renderEndScreen rootSurf won = do
  fillSurf (if won then 0x0000ffff else 0xff0000ff) rootSurf
  font' <- font
  SDLttf.renderTextSolid font' (message won) $ SDL.Color 0 0 0
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
