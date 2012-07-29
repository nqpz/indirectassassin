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

module IndirectAssassin.BaseTypes where

-- Global
import Prelude hiding (Right, Left)
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDLi
import Data.Char
-- Local
import IndirectAssassin.Misc

data Direction = Up | Left | Down | Right
               deriving (Show, Read, Eq, Ord)

stringToDirection :: String -> Direction
stringToDirection (c : cs) = read $ toUpper c : cs

instance Enum Direction where
  succ Up    = Left
  succ Left  = Down
  succ Down  = Right
  succ Right = Up

  toEnum 0 = Up
  toEnum 1 = Left
  toEnum 2 = Down
  toEnum 3 = Right
  toEnum n = toEnum $ n `posrem` 4
  
  fromEnum Up    = 0
  fromEnum Left  = 1
  fromEnum Down  = 2
  fromEnum Right = 3

type Position = (Int, Int)
type StillVector = (Direction, Position)

type SurfPart = (SDL.Surface, SDL.Rect)


type CenterList a = ([a], [a])

nextElement :: CenterList a -> a -> (CenterList a, a)
nextElement (before, (next : after)) current = ((current : before, after), next)

prevElement :: CenterList a -> a -> (CenterList a, a)
prevElement ((prev : before), after) current = ((before, current : after), prev)

createInfCenterList :: [a] -> (CenterList a, a)
createInfCenterList xs'@(x : xs) = ((cycle $ reverse xs', cycle (xs ++ [x])), x)


data Item = Barrels   -- A
          | Buckets   -- U
          | YellowBat -- E
          | GreenBee  -- R
          | Diamond   -- I
          | Tomato    -- O
          | IceShield -- C
          | Toilet    -- L
          | TurnAtWall { getTurnDirection :: Direction
                       }
          deriving (Show, Eq, Ord)

isTurnDirection :: Item -> Bool
isTurnDirection (TurnAtWall _) = True
isTurnDirection _ = False

data Cell = Wall
          | Empty
          | Professor { getDirection :: Direction 
                      , getItems :: [(Item, Int)]
                      }
          | Agent  { getDirection :: Direction 
                   , getItems :: [Item]
                   }
          | Item { getItem :: Item
                 }
          deriving (Show, Eq)

type Game = Map.Map Position Cell

data AgentAction = Go Direction | UseItem Item | PassTurn
                 deriving (Show, Eq)

data UserAction = NoAction | PrevGame | NextGame | PrevMap | NextMap 
                | ToggleCheat | Accept | AgentAction AgentAction 
                | NewDirection Direction | ExitGame
                deriving (Show, Eq)

data StepEffect = NewGame | NoChange | GameWon Bool
                deriving (Show, Eq)

data GameExtra = GameExtra { getGame :: Game
                           , hasWon :: Maybe Bool
                           , isCheating :: Bool
                           , getOrigGame :: Game
                           }
               deriving (Show, Eq)

data Lighting = Darkness | Flashlight | NightVision
              deriving (Show, Eq)

(charToItem, itemToChar) = (lookupOn alist, lookupOn $ map (\(a, b) -> (b, a)) alist)
  where alist = [('A', Barrels), ('U', Buckets), ('E', GreenBee),
                 ('I', Diamond), ('O', Tomato), ('C', IceShield),
                 ('L', Toilet)]
        lookupOn alist x = Map.lookup x $ Map.fromList alist

stringToItem :: String -> Item
stringToItem ('t':'u':'r':'n':'-':cs) = TurnAtWall $ stringToDirection cs
stringToItem (c : cs) | null cs = maybe (error "no parse") id $ charToItem c

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

cellAt game p = maybe Empty id $ Map.lookup p game

calcOffset :: Direction -> (Int, Int)
calcOffset Up    = (0, -1)
calcOffset Left  = (-1, 0)
calcOffset Down  = (0, 1)
calcOffset Right = (1, 0)
