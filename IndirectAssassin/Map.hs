{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IndirectAssassin.Map where

-- Global
import Prelude hiding (Right, Left)
import Data.Maybe
import qualified Data.Map as Map
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Misc

data Item = Barrels   -- A
          | Buckets   -- U
          | YellowBat -- E
          | GreenBee  -- R
          | Diamond   -- I
          | Tomato    -- O
          | IceShield -- C
          | TurnAtWall Direction
          deriving (Show, Eq)

charToItem 'A' = Just Barrels
charToItem 'U' = Just Buckets
charToItem 'E' = Just YellowBat
charToItem 'R' = Just GreenBee
charToItem 'I' = Just Diamond
charToItem 'O' = Just Tomato
charToItem 'C' = Just IceShield
charToItem _   = Nothing

instance Read Item where
  read ('t':'u':'r':'n':'-':cs) = TurnAtWall $ read cs :: Direction
  read (c : cs) | null cs = maybe (error "no parse") charToItem c

data Cell = Wall
          | Empty
          | Professor { getDirection :: Direction 
                      , getItems :: [Item]
                      }
          | Agent  { getDirection :: Direction 
                   , getItems :: [Item]
                   }
          | Item { getItem :: Item
                 }
          deriving (Show)

type Game = Map.Map Position Cell

isProfessor :: Cell -> Bool
isProfessor (Professor _ _) = True
isProfessor _               = False

isAgent :: Cell -> Bool
isAgent (Agent _ _) = True
isAgent _           = False

isItem :: Cell -> Bool
isItem (Item _) = True
isItem _        = False

isWall :: Cell -> Bool
isWall Wall = True
isWall _    = False

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False



-- 
nextPosition :: Direction -> Direction -> Position -> (Direction, Position)
nextPosition Up rd (x, y) = (rd, p')
  where p' = case rd of
          Up    -> (x, y - 1)
          Right -> (x + 1, y)
          Down  -> (x, y + 1)
          Left  -> (x - 1, y)
nextPosition d rd p = nextPosition (pred d) (succ rd) p

cellAt gMap p = maybe Empty id $ Map.lookup p gMap

