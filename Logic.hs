module Logic where

-- Global
import qualified Data.Map as Map
-- Local
import Misc
import GameMap

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

moveUser :: Game -> Direction -> Game
moveUser (gMap, userPos) dir = if isWall cell then (gMap, userPos) 
                               else (Map.insert newPos newUser nMap, newPos)
  where (User _ items) = gMap Map.! userPos
        (_, newPos) = nextPosition dir Up userPos
        cell = cellAt gMap newPos
        nMap = Map.delete userPos gMap
        newUser = User dir (if isItem cell then getItem cell : items else items)

nextStep :: Game -> Maybe Game
nextStep (gMap, userPos) = Just (withGuardsMoved, userPos)
  where withGuardsMoved = Map.foldWithKey moveGuards gMap $ Map.filter isGuard gMap
        moveGuards p (Guard dir mvStrategy items) gMap
          = Map.insert newPos newGuard nMap
            where nMap = Map.delete p gMap
                  (newDir, newPos) = mvStrategy nMap (dir, p)
                  cell = cellAt nMap newPos
                  newGuard = Guard newDir mvStrategy (if isItem cell then getItem cell : items else items)
