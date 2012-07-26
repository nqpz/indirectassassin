module IndirectAssassin.Logic where

-- Global
import qualified Data.Map as Map
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map


nextDirPos :: Direction -> Direction -> Position -> (Direction, Position)
nextDirPos Up rd (x, y) = (rd, p')
  where p' = case rd of
          Up    -> (x, y - 1)
          Right -> (x + 1, y)
          Down  -> (x, y + 1)
          Left  -> (x - 1, y)
nextDirPos d rd p = nextDirPos (pred d) (succ rd) p


step :: Game -> AgentAction -> (StepEffect, Game)
step game action = case action of
  UseItem item -> if isEmpty $ cellAt nextPos 
                  then (NoChange, game)
                  else runAI $ Map.insert nextPos item
    where nextPos = snd $ nextDirPos agentDir Up agentPos
  Go nextDir -> case cellAt nextPos of
    Empty -> runAI $ Map.insert nextPos (Agent nextDir agentItems) $ Map.delete agentPos
    Item item -> runAI $ Map.insert nextPos (Agent nextDir (item : agentItems)) $ Map.delete agentPos
    Wall -> (NoChange, game)
    Professor _ _ -> (LostGame, game)
    where (_, nextPos) = nextDirPos nextDir Up agentPos
  where (agentPos, Agent agentDir agentItems) = filter (isAgent . snd) $ Map.toList game

runAI :: Game -> (StepEffect, Game)
runAI game = 






turn :: Direction -> GameMap -> StillVector -> StillVector
turn relDir gMap (dir, p) = case cellAt gMap nextPos of
  Empty  -> (nextDir, nextPos)
  Item _ -> (nextDir, nextPos)
  _     -> case cellAt gMap altPos of
    Empty  -> (altDir, altPos)
    Item _ -> (altDir, altPos)
    _     -> (dir, p)
  where (nextDir, nextPos) = (nextDirPos dir Up p)
        (altDir, altPos)   = (nextDirPos dir relDir p)

moveUser :: Game -> Direction -> (Game, Map.Map Position Position)
moveUser (gMap, userPos) dir = if isWall cell || isGuard cell then ((gMap, userPos), Map.empty)
                               else ((Map.insert newPos newUser nMap, newPos), 
                                     Map.insert newPos userPos Map.empty)
  where (User _ items) = gMap Map.! userPos
        (_, newPos) = nextDirPos dir Up userPos
        cell = cellAt gMap newPos
        nMap = Map.delete userPos gMap
        newUser = User dir (if isItem cell then getItem cell : items else items)

nextStep :: Game -> Maybe (Game, Map.Map Position Position)
nextStep (gMap, userPos) = Just ((withGuardsMoved, userPos), posChanged)
  where (withGuardsMoved, posChanged) = Map.foldWithKey moveGuards (gMap, Map.empty) $ Map.filter isGuard gMap
        moveGuards p (Guard dir mvStrategy items) (gMap, pMap)
          = if p /= newPos then (Map.insert newPos newGuard nMap, Map.insert newPos p pMap)
            else (gMap, pMap)
            where nMap = Map.delete p gMap
                  (newDir, newPos) = mvStrategy nMap (dir, p)
                  cell = cellAt nMap newPos
                  newGuard = Guard newDir mvStrategy (if isItem cell then getItem cell : items else items)
