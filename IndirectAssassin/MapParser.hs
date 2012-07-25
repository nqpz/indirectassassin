module IndirectAssassin.MapParser where

-- Global
import Prelude hiding (Right, Left)
import Data.List (foldl')
import qualified Data.Map as Map
-- Local
import IndirectAssassin.Misc
import IndirectAssassin.BaseTypes
import IndirectAssassin.Map

parseGameMap :: String -> Game
parseGameMap cs = getMap top $ getMeta bottom
  where (top, bottom) = (splitSub "\n\n" cs)
        
        getMeta = foldl' makeMeta Map.empty . map words . lines
          where 
            makeMeta metaMap [[identifier], direction, items]
              = Map.insert identifier (t direction $ map charToItem items) metaMap
                where t = case identifier of 
                        '!' -> Agent
                        _   -> Professor
        
        getMap top meta = foldl' build ((0, 0), Map.empty) top
          where build ((_, y), game) '\n' = ((0, y + 1), game)
                build (p@(x, y), game) c  = ((x + 1, y), Map.insert (getCell c) x game)
                
                getCell '#' = Wall
                getCell ' ' = Empty
                getCell c   = maybe (maybe (error "no such item") id . charToItem) id Map.lookup c meta

loadGameMap :: String -> IO Game
loadGameMap filePath = do
  contents <- readFile filePath
  return $ parseGameMap contents
