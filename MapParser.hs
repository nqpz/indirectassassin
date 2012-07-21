module MapParser where

-- Global
import Prelude hiding (Right, Left)
import Data.List (foldl')
import qualified Data.Map as Map
-- Local
import Misc
import GameMap
import Logic

parseGameMap :: String -> Game
parseGameMap cs = (gMap, userPos)
  where (top, bottom) = (splitSub "\n\n" cs)
        parseTop meta = gMap
          where (_, gMap) = foldl' build ((0, 0), Map.empty) top
                build ((_, y), gMap) '\n' = ((0, y + 1), gMap)
                build (p@(x, y), gMap) c  = ((x + 1, y), case getType c of 
                                                Empty -> gMap
                                                x     -> Map.insert p x gMap)
                getType '#' = Wall
                getType ' ' = Empty
                getType '!' = let Guard dir _ _ = meta Map.! '!'
                              in User dir []
                getType x   = case getItemType x of
                  Just t  -> Item t
                  Nothing -> meta Map.! x
                getItemType 'f' = Just Fish
                getItemType 'm' = Just Meat
                getItemType _ = Nothing

        dirs = Map.fromList [("up", Up), ("right", Right), ("down", Down), ("left", Left)]
        mvStrategies = Map.fromList [("turn-right", turn Right), ("turn-left", turn Left), ("turn-around", turn Down)]
        createGuard args = Guard getDirection getMovementStrategy []
          where getDirection = translate dirs args Down
                getMovementStrategy = translate mvStrategies args $ turn Right

        parseBottom = foldl' makeMap Map.empty $ map words $ lines bottom
        makeMap metaMap (what : args) = Map.insert (head what) (createGuard args) metaMap
        makeMap metaMap [] = metaMap
        gMap = parseTop parseBottom
        userPos = fst $ head $ filter (isUser . snd) $ Map.toList gMap

loadGameMap :: String -> IO Game
loadGameMap filePath = do
  contents <- readFile filePath
  return $ parseGameMap contents
