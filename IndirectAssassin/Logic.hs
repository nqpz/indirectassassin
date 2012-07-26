module IndirectAssassin.Logic where

-- Global
import qualified Data.Map as Map
-- Local
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
  UseItem item -> if isEmpty $ cellAt nextPos 
                  then (NoChange, game)
                  else runAI $ Map.insert nextPos item
    where nextPos = snd $ nextDirPos agentDir Up agentPos
  Go nextDir -> case cellAt nextPos of
    Empty -> runAI $ Map.insert nextPos (Agent nextDir agentItems) $ Map.delete agentPos game
    Item item -> runAI $ Map.insert nextPos (Agent nextDir (item : agentItems)) $ Map.delete agentPos game
    Wall -> (NoChange, game)
    where (_, nextPos) = nextDirPos nextDir Up agentPos
  where (agentPos, Agent agentDir agentItems) = filter (isAgent . snd) $ Map.toList game

profLightLength dir (x, y) items = 3

profSprite dir (x, y) items = professor

profNextDirPos dir (x, y) items = (dir, (x, y))

gameOverWon :: Game -> Maybe Bool
gameOverWon game = Nothing

runAI :: Game -> (StepEffect, Game)
runAI game = whenNotOver game $ runAI' game
  where runAI' game = whenNotOver newGame $ (NewGame, newGame)
        newGame = Map.foldWithKey buildNew game
        buildNew (p, Professor dir items) game = case cellAt nextPos of
          Empty -> Map.insert nextPos (Professor nextDir items) $ Map.delete p game
          Item item -> Map.insert nextPos (Professor nextDir (item : items)) $ Map.delete p game
          Wall -> game
          Professor _ _ -> game
          where (nextDir, nextPos) = profNextDirPos dir p items
        buildNew (_, _) game = game
        whenNotOver game f = maybe f (\b -> (GameWon b, game)) $ gameOverWon game

