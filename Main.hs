module Main(main) where

-- Global
import System.Environment
-- Local
import MapParser (loadGameMap)
import GameGraphics

main :: IO ()
main = do
  args <- getArgs
  if (\x -> x == "--help" || x == "-h") `any` args
    then putStrLn "help text" >> return ()
    else runGame $ map loadGameMap args

