module IndirectAssassin.Logic where

-- Global
import Prelude hiding (Right, Left)
import Data.Word
import qualified Data.Map as Map
import Data.List (foldl', delete, find)
import Control.Monad
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Misc
import IndirectAssassin.Map
import IndirectAssassin.Graphics


nextDirPos :: Direction -> Direction -> Position -> (Direction, Position)
nextDirPos Up rd (x, y) = (rd, p')
  where p' = case rd of
          Up    -> (x, y - 1)
          Right -> (x + 1, y)
          Down  -> (x, y + 1)
          Left  -> (x - 1, y)
nextDirPos d rd p = nextDirPos (pred d) (succ rd) p


step :: Game -> AgentAction -> (StepEffect, Game, Map.Map Position Position)
step game action = case action of
  UseItem item -> if (not $ item `elem` agentItems)
                     || (not $ isEmpty $ cellAt game nextPos)
                  then (NoChange, game, Map.empty)
                  else runAI (Map.insert nextPos (Item item) (Map.insert agentPos (Agent agentDir $ delete item agentItems) game)) $ posChanges nextPos
    where nextPos = snd $ nextDirPos agentDir Up agentPos
  Go nextDir -> case cellAt game nextPos of
    Empty -> runAI (Map.insert nextPos (Agent nextDir agentItems) $ Map.delete agentPos game) $ posChanges nextPos
    Item item -> runAI (Map.insert nextPos (Agent nextDir (item : agentItems)) $ Map.delete agentPos game) $ posChanges nextPos
    Wall -> (NoChange, game, Map.empty)
    Professor _ _ -> (GameWon False, game, Map.empty)
    where (_, nextPos) = nextDirPos nextDir Up agentPos
  where (agentPos, Agent agentDir agentItems) = head $ filter (isAgent . snd) $ Map.toList game
        posChanges nextPos = (Map.insert nextPos agentPos Map.empty)

profLightLength :: Direction -> Position -> [Item] -> Int
profLightLength dir (x, y) items = 3

profSprite :: Graphics -> Direction -> Position -> [Item] -> (Word32 -> Word32 -> SurfPart, SurfPart)
profSprite g dir (x, y) items = getProfessor g dir

profNextDirPos :: Game -> Direction -> Position -> [Item] -> Maybe (Direction, Position)
profNextDirPos game dir pos items | not $ [isEmpty, isItem] `anytrue` cellAt game nextPos = next'
                                  | otherwise = Just (nextDir, nextPos)
  where (nextDir, nextPos) = nextDirPos dir Up pos
        next' = liftM (\t -> nextDirPos dir t pos) turnAt
        turnAt :: Maybe Direction
        turnAt = liftM getTurnDirection $ find isTurnDirection items

profIsDead :: Direction -> Position -> [Item] -> Bool
profIsDead dir (x, y) items = Tomato `elem` items

gameOverWon :: Game -> Maybe Bool
gameOverWon game = if null $ filter (\(pos, c) -> isProfessor c && (not $ profIsDead (getDirection c) pos (getItems c))) $ Map.toList game
                   then Just True
                   else if gameLost then Just False else Nothing
  where gameLost :: Bool
        gameLost = maybe False (== Flashlight) $ Map.lookup (fst $ getGameAgent game) (getLighting game)

runAI :: Game -> Map.Map Position Position -> (StepEffect, Game, Map.Map Position Position)
runAI game posChanges = whenNotOver game $ runAI' game
  where runAI' game = whenNotOver (fst newGame) (NewGame, fst newGame, snd newGame)
        newGame :: (Game, Map.Map Position Position)
        newGame = foldl' buildNew (game, posChanges) $ Map.toList game
        buildNew state@(game, posChanges) (p, Professor dir items)
          | isDead = (Map.delete p game, posChanges)
          | ndp == Nothing = state
          | otherwise = actOnNext ndp
          where isDead = profIsDead dir p items
                ndp = (profNextDirPos game dir p items)
                actOnNext (Just (nextDir, nextPos)) = case cellAt game nextPos of
                  Empty -> fill items
                  Item item -> fill $ item : items
                  Wall -> state
                  Professor _ _ -> state
                  where fill items = (Map.insert nextPos (Professor nextDir items) $ Map.delete p game,
                                      Map.insert nextPos p posChanges)
        buildNew state _ = state
        whenNotOver game f = maybe f (\b -> (GameWon b, game, posChanges)) $ gameOverWon game


getGameAgent game = head $ filter (isAgent . snd) $ Map.toList game

lightFrom :: Direction -> Position -> Int -> [Position]
lightFrom _   _   (-1) = []
lightFrom dir pos n    = pos : lightFrom dir npos (n - 1)
  where (_, npos) = nextDirPos dir Up pos

getFlashlightTiles game = foldl' buildLight Map.empty $ filter (isProfessor . snd) $ Map.toList game
  where buildLight lightMap (pos, Professor dir items) = foldl' build lightMap $ lightFrom dir pos $ profLightLength dir pos items
        build lightMap pos = Map.insert pos Flashlight lightMap

getNightVisionTiles game = buildLight $ getGameAgent game
  where buildLight (pos, Agent dir items) = foldl' build Map.empty $ lightFrom dir pos 3
        build lightMap pos = Map.insert pos NightVision lightMap

getLighting :: Game -> Map.Map Position Lighting
getLighting game = Map.union (getFlashlightTiles game) (getNightVisionTiles game)
