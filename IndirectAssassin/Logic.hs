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
import Data.Maybe
import qualified Data.Map as Map
import Data.List (foldl', delete, find, isInfixOf)
import Data.List.Utils (countElem)
import Control.Monad
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Misc


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
profItemLast Diamond   = -1
profItemLast Tomato    = 7
profItemLast IceShield = 3
profItemLast Toilet    = 10
profItemLast (TurnAtWall _) = -1

itemWithLast :: Item -> (Item, Int)
itemWithLast = (\(a, b) x -> (a x, b x)) (id, profItemLast)

profLightLength :: Direction -> Position -> [Item] -> Int
profLightLength dir (x, y) items | Buckets `elem` items = 5
                                 | otherwise = 3

profSprite :: Graphics -> Direction -> Position -> [Item] -> (Word32 -> SurfPart, SurfPart)
profSprite g dir (x, y) items | YellowBat `elem` items = getSoldierZombie g dir
                              | Tomato `elem` items = getSoldierNormal g dir
                              | otherwise = getProfessor g dir

profNextDirPos :: Game -> Direction -> Position -> [Item] -> Maybe StillVector
profNextDirPos game dir pos@(x, y) items | Toilet `elem` items = Nothing
                                         | Tomato `elem` items = nhdp isAgent
                                         | Barrels `elem` items = nhdp isProfessor
                                         | not $ [isEmpty, isItem] `anytrue` cellAt game nextPos = next'
                                         | otherwise = Just (nextDir, nextPos)
  where (nextDir, nextPos) = nextDirPos dir Up pos
        next' = liftM (\t -> nextDirPos dir t pos) turnAt
        turnAt :: Maybe Direction
        turnAt = liftM (perhapsRev . getTurnDirection) $ find isTurnDirection items
        perhapsRev = (2 * ((Diamond `countElem` items) `rem` 2)) .< succ
        nhdp check = liftM (\d -> nextDirPos d Up pos) huntNext
          where huntNext | x < x' = Just Right
                         | x > x' = Just Left
                         | y < y' = Just Down
                         | y > y' = Just Up
                         | otherwise = Nothing
                (x', y') = fst $ head $ filter (check . snd) $ Map.toList game

profIsDead :: Game -> Direction -> Position -> [Item] -> Bool
profIsDead game dir pos@(x, y) items = (GreenBee `elem` items)
                                       || ((pos `elem` zombieLight)
                                           && (not (ownLight `isInfixOf` zombieLight)))
  where ownLight = lightExtend (pos, Professor dir $ map itemWithLast items)
        zombieLight = concatMap lightExtend zombies
        zombies = filter (\(_, c) -> isProfessor c && Tomato `elem` (map fst $ getItems c)) $ Map.toList game
        lightExtend (pos, Professor dir items) = lightFrom dir pos $ profLightLength dir pos $ map fst items

lightFrom :: Direction -> Position -> Int -> [Position]
lightFrom _   _   (-1) = []
lightFrom dir pos n    = pos : lightFrom dir npos (n - 1)
  where (_, npos) = nextDirPos dir Up pos

getFlashlightTiles game = foldl' buildLight Map.empty $ filter (isProfessor . snd) $ Map.toList game
  where buildLight lightMap (pos, Professor dir items) = foldl' build lightMap $ lightFrom dir pos $ profLightLength dir pos $ map fst items
        build lightMap pos = Map.insert pos Flashlight lightMap

getNightVisionTiles game = buildLight $ getGameAgent game
  where buildLight (pos, Agent dir items) = foldl' build Map.empty $ lightFrom dir pos 3
        build lightMap pos = Map.insert pos NightVision lightMap

getLighting :: Game -> Map.Map Position Lighting
getLighting game = Map.union (getFlashlightTiles game) (getNightVisionTiles game)

gameOverWon :: Game -> Maybe Bool
gameOverWon game = maybe (Just True) (const elseExpr) $ find isAliveProf $ Map.toList game
  where isAliveProf (pos, c) = isProfessor c && (not $ profIsDead game (getDirection c) pos (map fst $ getItems c))
        gameLost :: Bool
        gameLost = maybe False (== Flashlight) $ Map.lookup (fst $ getGameAgent game) (getLighting game)
        elseExpr | gameLost = Just False 
                 | otherwise = Nothing


runAI :: Game -> Map.Map Position Position -> (StepEffect, Game, Map.Map Position Position)
runAI game posChanges = whenNotOver game $ runAI' game
  where runAI' game = whenNotOver (fst newGame) (NewGame, fst newGame, snd newGame)
        newGame :: (Game, Map.Map Position Position)
        newGame = foldl' buildNew (game, posChanges) $ Map.toList game
        buildNew (game, posChanges) (p, Professor dir items)
          | isDead = (Map.delete p game, posChanges)
          | ndp == Nothing = nothingNew
          | otherwise = actOnNext ndp
          where isDead = profIsDead game dir p items'
                ndp = profNextDirPos game dir p items'
                actOnNext (Just (nextDir, nextPos)) = case cellAt game nextPos of
                  Empty -> fill []
                  Item item -> if IceShield `elem` items'
                               then fill []
                               else fill [item]
                  Wall -> withNewDir
                  Professor _ _ -> nothingNew
                  where fill newItems = (Map.insert nextPos (Professor nextDir itemsOldNew) $ Map.delete p game,
                                                  Map.insert nextPos p posChanges)
                          where itemsOldNew = map itemWithLast newItems ++ (catMaybes $ map adjust items)
                        withNewDir = (Map.insert p (Professor nextDir (catMaybes $ map adjust items)) game,
                                      posChanges)
                items' = map fst items
                nothingNew = (Map.insert p (Professor dir (catMaybes $ map adjust items)) game,
                              posChanges)
        buildNew state _ = state
        whenNotOver game f = maybe f (\b -> (GameWon b, game, posChanges)) $ gameOverWon game
        adjust (item, 1) = Nothing
        adjust orig@(item, -1) = Just orig
        adjust (item, n) = Just (item, n - 1)


getGameAgent game = head $ filter (isAgent . snd) $ Map.toList game
