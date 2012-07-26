module Main (main) where

-- Global
import System.Environment
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map (loadGameMap)
import IndirectAssassin.GameRunner

main :: IO ()
main = do
  args <- getArgs
  games <- outM $ map loadGameMap args
  if ["--help", "-h"] `anyelem` args
    then printHelp
    else runGames games

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [MAP]...\n\n\
\Indirect Assassin is a turn-based stealth game.\n\
\To play, you should read the documentation README."
