{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module GameMap where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import qualified Data.Map as Map
-- Local
import Misc


data Direction = Up | Right | Down | Left
               deriving (Enum, Show, Eq)

nextDir Left = Up
nextDir d    = succ d
prevDir = 3 .< nextDir

type Position = (Int, Int)
type StillVector = (Direction, Position)
type MovementStrategy = GameMap -> StillVector -> StillVector

instance Show MovementStrategy where
  show f = "<function>"

data ItemType = Fish | Meat
              deriving (Show, Eq)

data Cell = Wall
          | Empty
          | Guard Direction MovementStrategy [ItemType]
          | User Direction [ItemType]
          | Item ItemType
          deriving (Show)

type GameMap = Map.Map Position Cell
type Game = (GameMap, Position)

translate :: Ord k => Map.Map k v -> [k] -> v -> v
translate m [] dfault = dfault
translate m (k : ks) dfaults = maybe (translate m ks dfaults) id $ Map.lookup k m

nextPosition :: Direction -> Direction -> Position -> (Direction, Position)
nextPosition Up rd (x, y) = (rd, p')
  where p' = case rd of
          Up    -> (x, y - 1)
          Right -> (x + 1, y)
          Down  -> (x, y + 1)
          Left  -> (x - 1, y)
nextPosition d rd p = nextPosition (prevDir d) (nextDir rd) p

cellAt gMap p = maybe Empty id $ Map.lookup p gMap

