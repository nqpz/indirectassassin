{--
Indirect Assassin: a turn-based stealth game
Copyright (C) 2012  Niels G. W. Serup

This file is part of Indirect Assassin.

Indirect Assassin is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Indirect Assassin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
Indirect Assassin.  If not, see <http://www.gnu.org/licenses/>.
--}

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
-- import qualified Graphics.UI.SDL.Mixer as SDLmix
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
startCount = (writeIORef theStartTime =<< SDL.getTicks) 
             >> writeIORef theFrameCount 0

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


runGames :: [(Game, String)] -> IO ()
runGames games = do
  SDL.init [SDL.InitEverything]
  SDLttf.init
  SDL.setVideoMode width height 32 []
  SDL.setCaption "Indirect Assassin" "indirectassassin"

  graphics <- getGraphics
  -- print (getBackgroundMusic graphics)
  -- SDLmix.playMusic (getBackgroundMusic graphics) 2
  screenSurf <- SDL.getVideoSurface
  let gameLists = map (\(game, name) -> 
                        createInfCenterList 
                        [GameExtra game Nothing False game name]) games
  let (gameListLists, (currentGameList, currentGame))
        = createInfCenterList gameLists
  startCount
  putStrLn "Start."
  gamesLoop screenSurf graphics (gameListLists, (currentGameList, currentGame))
  putStrLn "End."
  closeGraphics graphics
  SDLttf.quit
  SDL.quit


gamesLoop :: SDL.Surface
             -> Graphics
             -> (CenterList (CenterList GameExtra, GameExtra),
                 (CenterList GameExtra, GameExtra))
             -> IO ()
gamesLoop rootSurf graphics args@(gameListLists, 
                                 (currentGameList, currentGame)) = do
  event <- SDL.pollEvent
  case event of 
    SDL.NoEvent -> rnd currentGame >> loop args
    event -> case eventAction event of
      Nothing -> rnd currentGame >> loop args
      Just t -> case t of
        PrevGame -> (rnd $ snd prevGame) >> loop (gameListLists, prevGame)
        NextGame -> (rnd $ snd nextGame) >> loop (gameListLists, nextGame)
        PrevMap -> (rnd $ snd $ snd prevMap) >> loop prevMap
        NextMap -> (rnd $ snd $ snd nextMap) >> loop nextMap
        NewDirection direc -> loop $ newGame newGameExtra
          where newGameExtra = currentGame {
                  getGame = Map.insert agentPos newAgent $ getGame currentGame }
                newAgent = agent { getDirection = direc }
                (agentPos, agent) = getGameAgent $ getGame currentGame
        ToggleCheat -> rnd newGameExtra >> (loop $ newGame newGameExtra)
          where newGameExtra = currentGame {
                  isCheating = not $ isCheating currentGame }
        Accept -> maybe fallback (const action) $ hasWon currentGame
          where action = rnd newGameExtra >> (loop $ newGame newGameExtra)
                fallback = rnd currentGame >> loop args
                newGameExtra = currentGame { getGame = getOrigGame currentGame
                                           , hasWon = Nothing
                                           }
        AgentAction action -> do
          case hasWon currentGame of
            Just b -> rndEnd b >> loop args
            Nothing -> do
              case stepEffect of
                NoChange -> return ()
                NewGame -> rndInp currentGame newGameExtra posChanges
                GameWon b -> rndInp currentGame newGameExtra posChanges >> rndEnd b
              loop $ newGame newGameExtra
          where (stepEffect, game', posChanges) = (getGame currentGame)
                                                  `step` action
                hasWon' = case stepEffect of
                    GameWon b -> Just b
                    _ -> Nothing
                newGameExtra = currentGame { getGame = game'
                                           , hasWon = hasWon'
                                           }
        ExitGame -> return ()
  where
    rnd = render rootSurf graphics
    rndEnd = renderEndScreen rootSurf graphics
    rndInp = renderInterpolated rootSurf graphics
    loop = gamesLoop rootSurf graphics
    newGame g = (gameListLists, (currentGameList, g))
    prevGame = prevElement currentGameList currentGame
    nextGame = nextElement currentGameList currentGame
    prevMap  = prevElement gameListLists (currentGameList, currentGame)
    nextMap  = nextElement gameListLists (currentGameList, currentGame)

eventAction :: SDL.Event -> Maybe UserAction
eventAction (SDL.KeyDown (Keysym k mods c))
  | [KeyModCtrl, KeyModLeftCtrl, KeyModRightCtrl] `anyelem` mods = case k of
      SDLK_UP    -> Just PrevGame
      SDLK_DOWN  -> Just NextGame
      SDLK_LEFT  -> Just PrevMap
      SDLK_RIGHT -> Just NextMap
      SDLK_x     -> Just ToggleCheat
      _          -> Nothing
  | [KeyModShift, KeyModLeftShift, KeyModRightShift] `anyelem` mods = case k of
      SDLK_UP    -> Just $ NewDirection Up
      SDLK_DOWN  -> Just $ NewDirection Down
      SDLK_LEFT  -> Just $ NewDirection Left
      SDLK_RIGHT -> Just $ NewDirection Right
      _          -> Nothing
  | k == SDLK_UP     = Just $ AgentAction $ Go Up
  | k == SDLK_LEFT   = Just $ AgentAction $ Go Left
  | k == SDLK_DOWN   = Just $ AgentAction $ Go Down
  | k == SDLK_RIGHT  = Just $ AgentAction $ Go Right
  | k == SDLK_p      = Just $ AgentAction PassTurn
  | k == SDLK_ESCAPE = Just ExitGame
  | k == SDLK_RETURN || k == SDLK_KP_ENTER = Just Accept
  | k == SDLK_a = ui Barrels
  | k == SDLK_u = ui Buckets
  | k == SDLK_e = ui YellowBat
  | k == SDLK_r = ui GreenBee
  | k == SDLK_i = ui Diamond
  | k == SDLK_o = ui Tomato
  | k == SDLK_c = ui IceShield
  | k == SDLK_l = ui Toilet
  | otherwise = Nothing
  where ui = Just . AgentAction . UseItem
eventAction SDL.Quit = Just ExitGame
eventAction _ = Nothing

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
  let delay = max 0 $ 1000 * fromIntegral fn / fromIntegral fps 
              - fromIntegral msecs
  return $ delay * 1000

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
          if cheat 
            then drawLight rootSurf graphics game mapOffset 
            else drawDarkness rootSurf graphics game mapOffset
          drawItemChars rootSurf graphics game
          drawGameName rootSurf graphics $ getGameName gameExtra          
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)
                mapOffset = calcMapOffset game
                calcMapOffset game = (64, 64)
                                     * ((6, 4) - (fst $ getGameAgent game))

renderInterpolated :: SDL.Surface -> Graphics -> GameExtra -> GameExtra
                      -> Map.Map Position Position -> IO ()
renderInterpolated rootSurf graphics oldGameExtra gameExtra posChanges = do
  t <- SDL.getTicks
  renderAll t (t + 320) t
  where renderAll start end now = do
          let i = floor (fromIntegral (now - start) 
                         / (fromIntegral (end - start) / 8))
          renderOne i
          now' <- SDL.getTicks
          if now' < end
            then renderAll start end now'
            else return ()
        renderOne i = do
          blitFloor rootSurf graphics mapOffset
          drawItems rootSurf graphics game mapOffset
          drawProfessors rootSurf graphics game mapOffset posChanges i
          drawAgent rootSurf graphics game mapOffset posChanges i
          drawWalls rootSurf graphics game mapOffset
          if cheat 
            then drawLight rootSurf graphics (getGame oldGameExtra) mapOffset 
            else drawDarkness rootSurf graphics (getGame oldGameExtra) mapOffset
          drawItemChars rootSurf graphics game
          drawGameName rootSurf graphics $ getGameName gameExtra
          SDL.flip rootSurf
          waitForNextFrame
          where (game, cheat) = (getGame gameExtra, isCheating gameExtra)
                mapOffset = calcMapOffset game
                agentPos = fst $ getGameAgent game
                calcMapOffset game = (64, 64) * (
                  (6, 4) - maybe agentPos id (Map.lookup agentPos posChanges))


renderEndScreen :: SDL.Surface -> Graphics -> Bool -> IO ()
renderEndScreen rootSurf graphics won = do
  fillSurf (if won then 0x000000ff else 0x00ff0000) rootSurf
  textSurf <- SDLttf.renderTextSolid (getFont graphics) (message won)
              $ SDL.Color 255 255 255
  SDL.blitSurface textSurf Nothing rootSurf $ Just $ SDL.Rect 20 20 0 0
  SDL.flip rootSurf
  waitForNextFrame
  where message True  = "You have won."
        message False = "You have lost."

drawItems :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawItems surf graphics game mapOffset 
  = toUnit [ blitItem pos item | 
             (pos, Item item) <- filter (isItem . snd) $ Map.toList game ]
  where blitItem :: Position -> Item -> IO Bool
        blitItem (x, y) item = do
          t <- getTimePassed
          let (itemSurf, itemRect) = itemToImage graphics item t
          let offset = (floor $ fromIntegral (64 - SDL.rectW itemRect) / 2, 
                        floor $ fromIntegral (64 - SDL.rectH itemRect) / 2)
          SDL.blitSurface itemSurf (Just itemRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64 + fst offset) 
            (snd mapOffset + y * 64 + snd offset) 0 0
        
  
drawProfessors :: SDL.Surface -> Graphics -> Game -> (Int, Int) 
                  -> Map.Map Position Position -> Int -> IO ()
drawProfessors surf graphics game mapOffset posChanges i 
  = toUnit [ blitProf dir pos $ map fst items |
             (pos, Professor dir items) <- 
               filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          t <- getTimePassed
          let sprite = profSprite graphics dir pos items
          let ((profSurf, profRect), offset)
                = maybe (snd sprite, (0, 0)) 
                  (\op -> (((fst sprite) t), 
                           (-((7 - i) + 1) * 8, -((7 - i) + 1) * 8) 
                           * calcOffset dir)) (Map.lookup pos posChanges)
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64 + fst offset) 
            (snd mapOffset + y * 64 + snd offset) 64 64
        
drawProfessorsStill :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawProfessorsStill surf graphics game mapOffset
  = toUnit [ blitProf dir pos $ map fst items | 
             (pos, Professor dir items) <- 
               filter (isProfessor . snd) $ Map.toList game ]
  where blitProf :: Direction -> Position -> [Item] -> IO Bool 
        blitProf dir pos@(x, y) items = do
          let (profSurf, profRect) = snd $ profSprite graphics dir pos items
          SDL.blitSurface profSurf (Just profRect) surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64)
            (snd mapOffset + y * 64) 64 64

drawAgent :: SDL.Surface -> Graphics -> Game -> (Int, Int) 
             -> Map.Map Position Position -> Int -> IO ()
drawAgent surf graphics game mapOffset posChanges i = do
  let (p@(x, y), Agent dir items) = getGameAgent game
  t <- getTimePassed  
  let ((agentSurf, agentRect), offset)
        = maybe (snd $ (getAgent graphics) dir, (0, 0))
          (\op -> (((fst $ (getAgent graphics) dir) t),
                   (((7 - i) + 1) * 8, ((7 - i) + 1) * 8) * calcOffset dir))
          (Map.lookup p posChanges)
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (fst mapOffset + x * 64 - fst offset)
    (snd mapOffset + y * 64 - snd offset) 64 64
  return ()
    
drawAgentStill :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawAgentStill surf graphics game mapOffset = do
  let (pos@(x, y), Agent dir items) = head $ filter (isAgent . snd)
                                      $ Map.toList game
  let (agentSurf, agentRect) = snd $ (getAgent graphics) dir
  SDL.blitSurface agentSurf (Just agentRect) surf
    $ Just $ SDL.Rect (fst mapOffset + x * 64) (snd mapOffset + y * 64) 64 64
  return ()

drawWalls :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawWalls surf graphics game mapOffset
  = toUnit [ blitWall pos | 
             pos <- sortBy depth $ map fst 
                    $ filter (isWall . snd) $ Map.toList game ]
  where depth (_, y0) (_, y1) = compare y0 y1
        blitWall :: Position -> IO Bool
        blitWall (x, y) = do
          SDL.blitSurface (getWall graphics) Nothing surf
            $ Just $ SDL.Rect (fst mapOffset + x * 64)
            (snd mapOffset + y * 64 - 26) 64 88

drawDarkness :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawDarkness surf graphics game mapOffset
  = toUnit [ blitLighting (x, y) | x <- [0..11], y <- [0..8] ]
  where lighting = getLighting game
        blitLighting :: Position -> IO Bool
        blitLighting (x, y)
          = SDL.blitSurface (getLightingSurf graphics
                             $ maybe Darkness id $ Map.lookup 
                             (x - (floor $ fromIntegral (fst mapOffset) / 64),
                              y - (floor $ fromIntegral (snd mapOffset) / 64)) 
                             lighting) 
            Nothing surf $ Just $ SDL.Rect (x * 64) (y * 64) 64 64

drawLight :: SDL.Surface -> Graphics -> Game -> (Int, Int) -> IO ()
drawLight surf graphics game mapOffset
  = toUnit [ blitLighting (x, y) | 
             x <- [0..11], y <- [0..8] ]
  where lighting = getLighting game
        blitLighting :: Position -> IO Bool
        blitLighting (x, y)
          = maybe (return True) 
            (\l -> SDL.blitSurface (getLightingSurf graphics l)
                   Nothing surf $ Just $ SDL.Rect (x * 64) (y * 64) 64 64)
            $ Map.lookup (x - (floor $ fromIntegral (fst mapOffset) / 64),
                          y - (floor $ fromIntegral (snd mapOffset) / 64))
            lighting

getItemChars :: Game -> [Char]
getItemChars = catMaybes . map itemToChar . getAgentItems . snd . getGameAgent

drawItemChars :: SDL.Surface -> Graphics -> Game -> IO ()
drawItemChars rootSurf graphics game = do
  textSurf <- SDLttf.renderTextSolid (getFont graphics)
              ("Items " ++ (getItemChars game)) $ SDL.Color 200 30 0
  SDL.blitSurface textSurf Nothing rootSurf $ Just $ SDL.Rect 20 20 0 0
  return ()

drawGameName :: SDL.Surface -> Graphics -> String -> IO ()
drawGameName rootSurf graphics name = do
  textSurf <- SDLttf.renderTextSolid (getFont graphics) name
              $ SDL.Color 0 130 30
  SDL.blitSurface textSurf Nothing rootSurf $ Just $ SDL.Rect 20 520 0 0
  return ()

blitFloor :: SDL.Surface -> Graphics -> (Int, Int) -> IO ()
blitFloor rootSurf graphics mapOffset = do
  let floorS = getFloor graphics
  SDL.blitSurface floorS Nothing rootSurf
    $ Just $ SDL.Rect (fst mapOffset `posrem` 192 - 128) 0
    (SDL.surfaceGetWidth floorS) (SDL.surfaceGetHeight floorS)
  return ()