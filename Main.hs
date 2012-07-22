module Main(main) where

-- Global
import System.Environment
-- Local
import Misc
import MapParser (loadGameMap)
import GameGraphics

main :: IO ()
main = do
  args <- getArgs
  if ["--help", "-h"] `anyelem` args
    then putStrLn "help text" >> return ()
    else runGame $ map loadGameMap args

