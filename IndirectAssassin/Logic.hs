module IndirectAssassin.Logic where

-- Global
import Prelude hiding (Right, Left)
import Data.Word
import qualified Data.Map as Map
import Data.List (foldl')
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Misc
import IndirectAssassin.Map
import IndirectAssassin.Graphics


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
  UseItem item -> if isEmpty $ cellAt game nextPos 
                  then (NoChange, game)
                  else runAI $ Map.insert nextPos (Item item) game
    where nextPos = snd $ nextDirPos agentDir Up agentPos
  Go nextDir -> case cellAt game nextPos of
    Empty -> runAI $ Map.insert nextPos (Agent nextDir agentItems) $ Map.delete agentPos game
    Item item -> runAI $ Map.insert nextPos (Agent nextDir (item : agentItems)) $ Map.delete agentPos game
    Wall -> (NoChange, game)
    where (_, nextPos) = nextDirPos nextDir Up agentPos
  where (agentPos, Agent agentDir agentItems) = head $ filter (isAgent . snd) $ Map.toList game

profLightLength :: Direction -> (Int, Int) -> [Item] -> Int
profLightLength dir (x, y) items = 3

profSprite :: Direction -> (Int, Int) -> [Item] -> Word32 -> Word32 -> IO SurfPart
profSprite dir (x, y) items = professor dir

profNextDirPos :: Direction -> (Int, Int) -> [Item] -> (Direction, (Int, Int))
profNextDirPos dir (x, y) items = (dir, (x, y))

gameOverWon :: Game -> Maybe Bool
gameOverWon game = Nothing

runAI :: Game -> (StepEffect, Game)
runAI game = whenNotOver game $ runAI' game
  where runAI' game = whenNotOver newGame (NewGame, newGame)
        newGame :: Game
        newGame = foldl' buildNew game $ Map.toList game
        buildNew game (p, Professor dir items) = case cellAt game nextPos of
          Empty -> Map.insert nextPos (Professor nextDir items) $ Map.delete p game
          Item item -> Map.insert nextPos (Professor nextDir (item : items)) $ Map.delete p game
          Wall -> game
          Professor _ _ -> game
          where (nextDir, nextPos) = profNextDirPos dir p items
        buildNew game (_, _) = game
        whenNotOver game f = maybe f (\b -> (GameWon b, game)) $ gameOverWon game

