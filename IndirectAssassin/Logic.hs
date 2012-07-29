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
  PassTurn -> runAI game Map.empty
  where (agentPos, Agent agentDir agentItems) = head $ filter (isAgent . snd) $ Map.toList game
        posChanges nextPos = (Map.insert nextPos agentPos Map.empty)

profItemLast :: Item -> Int
profItemLast Barrels   = 1
profItemLast Buckets   = 6
profItemLast YellowBat = 4
profItemLast GreenBee  = 1
profItemLast Diamond   = 1
profItemLast Tomato    = 7
profItemLast IceShield = 3
profItemLast Toilet    = 10
profItemLast (TurnAtWall _) = -1

itemWithLast :: Item -> (Item, Int)
itemWithLast = (\(a, b) x -> (a x, b x)) (id, profItemLast)

profLightLength :: Direction -> Position -> [Item] -> Int
profLightLength dir (x, y) items | Buckets `elem` items = 5
                                 | otherwise = 3

profSprite :: Graphics -> Direction -> Position -> [Item] -> (Word32 -> Word32 -> SurfPart, SurfPart)
profSprite g dir (x, y) items | YellowBat `elem` items = getSoldierZombie g dir
                              | Tomato `elem` items = getSoldierNormal g dir
                              | otherwise = getProfessor g dir

profNextDirPos :: Game -> Direction -> Position -> [Item] -> Maybe (Direction, Position)
profNextDirPos game dir pos@(x, y) items | Tomato `elem` items = huntNext
                                         | not $ [isEmpty, isItem] `anytrue` cellAt game nextPos = next'
                                         | otherwise = Just (nextDir, nextPos)
  where (nextDir, nextPos) = nextDirPos dir Up pos
        next' = liftM (\t -> nextDirPos dir t pos) turnAt
        turnAt :: Maybe Direction
        turnAt = liftM (perhapsRev . getTurnDirection) $ find isTurnDirection items
        perhapsRev dir | Diamond `elem` items = succ $ succ dir
                       | otherwise = dir
        huntNext | x < x' = Up
                 | x > x' = Down
                 | y < y' = Right
                 | y > y' = Left
                 | otherwise = Up -- arbitrary
          where pos'@(x', y') = fst $ getGameAgent game

profIsDead :: Game -> Direction -> Position -> [Item] -> Bool
profIsDead game dir pos@(x, y) items = GreenBee `elem` items
                                       || pos `elem` zombieLight
  where zombieLight = concatMap lightExtend zombies
        zombies = filter (\(_, c) -> isProfessor c && Tomato `elem` (getItems c)) game
        lightExtend (pos, Professor dir items) = lightFrom dir pos $ profLightLength dir pos items

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

gameOverWon :: Game -> Maybe Bool
gameOverWon game = maybe (Just True) (const elseExpr) find isAliveProf Map.toList game
  where isAliveProf (pos, c) = isProfessor c && (not $ profIsDead game (getDirection c) pos (getItems c))
        gameLost :: Bool
        gameLost = maybe False (== Flashlight) $ Map.lookup (fst $ getGameAgent game) (getLighting game)
        elseExpr | gameLost = Just False 
                 | otherwise = Nothing


runAI :: Game -> Map.Map Position Position -> (StepEffect, Game, Map.Map Position Position)
runAI game posChanges = whenNotOver game $ runAI' game
  where runAI' game = whenNotOver (fst newGame) (NewGame, fst newGame, snd newGame)
        newGame :: (Game, Map.Map Position Position)
        newGame = foldl' buildNew (game, posChanges) $ Map.toList game
        buildNew state@(game, posChanges) (p, Professor dir items)
          | isDead = (Map.delete p game, posChanges)
          | ndp == Nothing = state
          | otherwise = actOnNext ndp
          where isDead = profIsDead game dir p items
                ndp = (profNextDirPos game dir p items)
                actOnNext (Just (nextDir, nextPos)) = case cellAt game nextPos of
                  Empty -> fill items []
                  Item item -> if IceShield `elem` items
                               then fill items []
                               else fill items item
                  Wall -> state
                  Professor _ _ -> state
                  where fill oldItems newItems = (Map.insert nextPos (Professor nextDir items) $ Map.delete p game,
                                                  Map.insert nextPos p posChanges)
                          where items = map itemWithLast newItems ++ catMaybes $ map adjust oldItems
                                adjust (item, 1) = Nothing
                                adjust orig@(item, -1) = orig
                                adjust (item, n) = (item, n - 1)
        buildNew state _ = state
        whenNotOver game f = maybe f (\b -> (GameWon b, game, posChanges)) $ gameOverWon game


getGameAgent game = head $ filter (isAgent . snd) $ Map.toList game
