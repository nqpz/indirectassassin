module Logic where

-- Global
import qualified Data.Map as Map
-- Local
import Misc
import GameMap

turn :: Direction -> GameMap -> StillVector -> StillVector
turn relDir gMap (dir, p) = case cellAt gMap nextPos of
  Empty -> (nextDir, nextPos)
  _     -> case cellAt gMap altPos of
    Empty -> (altDir, altPos)
    _     -> (dir, p)
  where (nextDir, nextPos) = (nextPosition dir Up p)
        (altDir, altPos)   = (nextPosition dir relDir p)

isGuard :: Cell -> Bool
isGuard (Guard _ _ _) = True
isGuard _             = False

isUser :: Cell -> Bool
isUser (User _ _) = True
isUser _          = False

moveUser :: Game -> Direction -> Game
moveUser (gMap, userPos) dir = (Map.insert newPos newUser nMap, newPos)
  where (User _ items) = gMap Map.! userPos
        nMap = Map.delete userPos gMap
        (_, newPos) = nextPosition dir Up userPos
        newUser = User dir items

nextStep :: Game -> Game
nextStep (gMap, userPos) = (withGuardsMoved, userPos)
  where withGuardsMoved = Map.foldWithKey moveGuards gMap $ Map.filter isGuard gMap
        moveGuards p (Guard dir mvStrategy items) gMap
          = Map.insert newPos newGuard nMap
            where nMap = Map.delete p gMap
                  (newDir, newPos) = mvStrategy nMap (dir, p)
                  newGuard = Guard newDir mvStrategy items
