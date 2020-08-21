module Main (main) where

-- Global
import System.Environment
import Paths_indirectassassin (getDataFileName)
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.Map (loadGameMap)
import IndirectAssassin.GameRunner

nMaps = 5

main :: IO ()
main = do
  args <- getArgs
  incMaps <- outM $ map (\s -> getDataFileName ("data/maps/" ++ s)) includedMaps
  if ["--help", "-h"] `anyelem` args
    then printHelp
    else runGames =<< (outM $ map loadGameMap (incMaps ++ (reverse args)))

includedMaps = [ "map" ++ show i ++ ".map" | i <- [0..nMaps - 1] ]

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [MAP]...\n\n\
\Indirect Assassin is a turn-based stealth game.\n\
\To play, you should read the documentation README."
