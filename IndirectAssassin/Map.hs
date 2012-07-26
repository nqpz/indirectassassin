{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IndirectAssassin.Map where

-- Global
import Prelude hiding (Right, Left)
import Data.List (foldl')
import qualified Data.List.Utils as U
import Data.Maybe
import qualified Data.Map as Map
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Misc

charToItem :: Char -> Maybe Item
charToItem 'A' = Just Barrels
charToItem 'U' = Just Buckets
charToItem 'E' = Just YellowBat
charToItem 'R' = Just GreenBee
charToItem 'I' = Just Diamond
charToItem 'O' = Just Tomato
charToItem 'C' = Just IceShield
charToItem _   = Nothing

stringToItem :: String -> Item
stringToItem ('t':'u':'r':'n':'-':cs) = TurnAtWall $ stringToDirection cs
stringToItem (c : cs) | null cs = maybe (error "no parse") id $ charToItem c

isProfessor :: Cell -> Bool
isProfessor (Professor _ _) = True
isProfessor _               = False

isAgent :: Cell -> Bool
isAgent (Agent _ _) = True
isAgent _           = False

isItem :: Cell -> Bool
isItem (Item _) = True
isItem _        = False

isWall :: Cell -> Bool
isWall Wall = True
isWall _    = False

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False

cellAt game p = maybe Empty id $ Map.lookup p game

parseGameMap :: String -> Game
parseGameMap cs = getMap top $ getMeta bottom
  where top : bottom : _ = U.split "\n\n" cs
        
        getMeta = foldl' makeMeta Map.empty . map words . lines
          where 
            makeMeta metaMap ([identifier] : direction : items)
              = Map.insert identifier (t (stringToDirection direction) $ map stringToItem items) metaMap
                where t = case identifier of 
                        '!' -> Agent
                        _   -> Professor
        
        getMap top meta = snd $ foldl' build ((0, 0), Map.empty) top
          where build ((_, y), game) '\n' = ((0, y + 1), game)
                build (p@(x, y), game) c  = ((x + 1, y), Map.insert p (getCell c) game)
                
                getCell '#' = Wall
                getCell ' ' = Empty
                getCell c   = maybe (maybe (error "no such item") Item $ charToItem c) id $ Map.lookup c meta

loadGameMap :: String -> IO Game
loadGameMap filePath = do
  contents <- readFile filePath
  return $ parseGameMap contents
