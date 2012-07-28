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
          | TurnAtWall { getTurnDirection :: Direction
                       }
          deriving (Show, Eq)

isTurnDirection :: Item -> Bool
isTurnDirection (TurnAtWall _) = True
isTurnDirection _ = False

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
          deriving (Show, Eq)

type Game = Map.Map Position Cell

data AgentAction = Go Direction | UseItem Item
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
