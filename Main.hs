module IndirectAssassin.Main (main) where

-- Global
import System.Environment
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map (loadGameMap)
import IndirectAssassin.GameRunner

main :: IO ()
main = do
  args <- getArgs
  if ["--help", "-h"] `anyelem` args
  then printHelp
  else runGames =<< outM $ map loadGameMap args

printHelp :: IO ()
printHelp = putStrLn "Usage: " +++ getProgName +++ " [MAP]...

Indirect Assassin is a turn-based stealth game.
To play, you should read the documentation README."
