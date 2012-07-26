module IndirectAssassin.BaseTypes where

-- Global
import Prelude hiding (Right, Left)
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
createInfCenterList xs'@(x : xs) = ((cycle $ reverse xs', cycle xs), x)


data AgentAction = GoUp | GoLeft | GoDown | GoRight | UseItem Item

data UserAction = NoAction | PreviousGame | NextGame | PreviousMap | NextMap 
                | ToggleCheat | Accept | AgentAction AgentAction | Redraw 
                | ExitGame
