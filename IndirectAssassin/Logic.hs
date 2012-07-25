module IndirectAssassin.Logic where

-- Global
import qualified Data.Map as Map
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map

turn :: Direction -> GameMap -> StillVector -> StillVector
turn relDir gMap (dir, p) = case cellAt gMap nextPos of
  Empty  -> (nextDir, nextPos)
  Item _ -> (nextDir, nextPos)
  _     -> case cellAt gMap altPos of
    Empty  -> (altDir, altPos)
    Item _ -> (altDir, altPos)
    _     -> (dir, p)
  where (nextDir, nextPos) = (nextPosition dir Up p)
        (altDir, altPos)   = (nextPosition dir relDir p)

moveUser :: Game -> Direction -> (Game, Map.Map Position Position)
moveUser (gMap, userPos) dir = if isWall cell || isGuard cell then ((gMap, userPos), Map.empty)
                               else ((Map.insert newPos newUser nMap, newPos), 
                                     Map.insert newPos userPos Map.empty)
  where (User _ items) = gMap Map.! userPos
        (_, newPos) = nextPosition dir Up userPos
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
