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
  closeGraphics graphics
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
            NextMap -> render rootSurf graphics (snd $ snd nextMap) >> gamesLoop rootSurf graphics nextMap
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
                      let (stepEffect, game', posChanges) = (getGame currentGame) `step` action
                      let hasWon' = case stepEffect of
                            GameWon b -> Just b
                            _ -> Nothing
                      let newGame = GameExtra game' hasWon' (isCheating currentGame) (getOrigGame currentGame)
                      case stepEffect of
                        NoChange -> return ()
                        NewGame -> renderInterpolated rootSurf graphics newGame posChanges
                        GameWon b -> renderInterpolated rootSurf graphics newGame posChanges >> renderEndScreen rootSurf graphics b
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

calculateDelay :: IO Double
calculateDelay = do
  msecs <- getTimePassed
  fn <- getFrameNumber
  fps <- getMaxFPS
  let delay = max 0 $ 1000 * fromIntegral fn / fromIntegral fps - fromIntegral msecs
  return $ delay * 1000

drawItems :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO [Bool]
drawItems surf graphics game mapOffset = outM [ blitItem pos item | (pos, Item item) <- filter (isItem . snd) $ Map.toList game ]
  where blitItem :: Position -> Item -> IO Bool
        blitItem (x, y) item = do
          fn <- getFrameNumber
          fps <- getFPS
          let (itemSurf, itemRect) = itemToImage graphics item (fn `rem` fps) fps
          let offset = (floor $ fromIntegral (64 - SDL.rectW itemRect) / 2, floor $ fromIntegral (64 - SDL.rectH itemRect) / 2)
          SDL.blitSurface itemSurf (Just itemRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64 + fst offset) (snd mapOffset + y * 64 + snd offset) (SDL.rectW itemRect) (SDL.rectH itemRect)
  
drawProfessors :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> Map.Map Position Position -> Int -> IO [Bool]
drawProfessors surf graphics game mapOffset posChanges i = outM [ blitProf dir pos items | (pos, Professor dir items) <- filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          fn <- getFrameNumber
          fps <- getFPS
          let sprite = profSprite graphics dir pos items
          let ((profSurf, profRect), offset) = maybe (snd sprite, (0, 0))
                                                 (\op -> (((fst sprite) (fn `rem` fps) fps),
                                                          ((i + 1) * 8, (i + 1) * 8) * calcOffset dir))
                                                 (Map.lookup pos posChanges)
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64 + fst offset) (snd mapOffset + y * 64 + snd offset) 64 64
        
drawProfessorsStill :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO [Bool]
drawProfessorsStill surf graphics game mapOffset = outM [ blitProf dir pos items | (pos, Professor dir items) <- filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          let (profSurf, profRect) = snd $ profSprite graphics dir pos items
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64) (snd mapOffset + y * 64) 64 64

getGameAgent game = head $ filter (isAgent . snd) $ Map.toList game

drawAgent :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> Map.Map Position Position -> Int -> IO Bool
drawAgent surf graphics game mapOffset posChanges i = do
  let (p@(x, y), Agent dir items) = getGameAgent game
  fn <- getFrameNumber
  fps <- getFPS
  let ((agentSurf, agentRect), offset) = maybe (snd $ (getAgent graphics) dir, (0, 0))
                                         (\op -> (((fst $ (getAgent graphics) dir) (fn `rem` fps) fps),
                                                  (((7 - i) + 1) * 8, ((7 - i) + 1) * 8) * calcOffset dir))
                                         (Map.lookup p posChanges)
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (fst mapOffset + x * 64 - fst offset) (snd mapOffset + y * 64 - snd offset) 64 64
    
calcOffset :: Direction -> (Int, Int)
calcOffset Up    = (0, -1)
calcOffset Left  = (-1, 0)
calcOffset Down  = (0, 1)
calcOffset Right = (1, 0)
            
drawAgentStill :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO Bool
drawAgentStill surf graphics game mapOffset = do
  let ((x, y), Agent dir items) = head $ filter (isAgent . snd) $ Map.toList game
  let (agentSurf, agentRect) = snd $ (getAgent graphics) dir
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (fst mapOffset + x * 64) (snd mapOffset + y * 64) 64 64

drawWalls :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO [Bool]
drawWalls surf graphics game mapOffset = outM [ blitWall pos | pos <- sortBy depth $ map fst $ filter (isWall . snd) $ Map.toList game ]
  where depth (_, y0) (_, y1) = compare y0 y1
        blitWall :: Position -> IO Bool
        blitWall (x, y) = do
          SDL.blitSurface (getWall graphics) Nothing surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64) (snd mapOffset + y * 64 - 26) 64 88

drawDarkness :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawDarkness surf graphics game mapOffset = do
  return ()

blitFloor :: SDL.Surface -> Graphics -> (Int, Int) -> IO Bool
blitFloor rootSurf graphics mapOffset = do
  SDL.blitSurface floorS Nothing rootSurf
    $ Just $ SDL.Rect (fst mapOffset `posrem` 192 - 128) 0
    (SDL.surfaceGetWidth floorS) (SDL.surfaceGetHeight floorS)
  where floorS =  getFloor graphics

render :: SDL.Surface -> Graphics -> GameExtra -> IO ()
render rootSurf graphics gameExtra = do
  case hasWon gameExtra of
    Nothing -> render'
    Just b -> renderEndScreen rootSurf graphics b
  where render' = do
          blitFloor rootSurf graphics mapOffset
          drawItems rootSurf graphics game mapOffset
          drawProfessorsStill rootSurf graphics game mapOffset
          drawAgentStill rootSurf graphics game mapOffset
          drawWalls rootSurf graphics game mapOffset
          if cheat then return () else drawDarkness rootSurf graphics game mapOffset
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)
                mapOffset = calcMapOffset game
                calcMapOffset game = (64, 64) * ((6, 4) - (fst $ getGameAgent game))

renderInterpolated :: SDL.Surface -> Graphics -> GameExtra -> Map.Map Position Position -> IO ()
renderInterpolated rootSurf graphics gameExtra posChanges = outM [ renderOne i | i <- [0..7] ] >> return ()
  where renderOne i = do
          blitFloor rootSurf graphics mapOffset
          drawItems rootSurf graphics game mapOffset
          drawProfessors rootSurf graphics game mapOffset posChanges i
          drawAgent rootSurf graphics game mapOffset posChanges i
          drawWalls rootSurf graphics game mapOffset
          if cheat then return () else drawDarkness rootSurf graphics game mapOffset
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)
                mapOffset = calcMapOffset game
                agentPos = fst $ getGameAgent game
                calcMapOffset game = (64, 64) * ((6, 4) - maybe agentPos id (Map.lookup agentPos posChanges))


renderEndScreen :: SDL.Surface -> Graphics -> Bool -> IO ()
renderEndScreen rootSurf graphics won = do
  fillSurf (if won then 0x0000ffff else 0xff0000ff) rootSurf
  SDLttf.renderTextSolid (getFont graphics) (message won) $ SDL.Color 0 0 0
  waitForNextFrame
  where message True  = "You have won!"
        message False = "You have lost!"

eventAction :: SDL.Event -> Maybe UserAction
eventAction (SDL.KeyDown (Keysym k mods c))
  | [KeyModCtrl, KeyModLeftCtrl, KeyModRightCtrl] `anyelem` mods = case k of 
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
