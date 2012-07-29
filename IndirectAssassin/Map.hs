{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{--
Indirect Assassin: a turn-based stealth game
Copyright (C) 2012  Niels G. W. Serup

This file is part of Indirect Assassin.

Indirect Assassin is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Indirect Assassin is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
Indirect Assassin.  If not, see <http://www.gnu.org/licenses/>.
--}

module IndirectAssassin.Map where

-- Global
import Prelude hiding (Right, Left)
import Data.List (foldl')
import qualified Data.List.Utils as U
import Data.Maybe
import qualified Data.Map as Map
-- Local
import IndirectAssassin.BaseTypes
import IndirectAssassin.Logic
import IndirectAssassin.Misc

parseGameMap :: String -> Game
parseGameMap cs = getMap top $ getMeta bottom
  where top : bottom : _ = U.split "\n\n" cs
        
        getMeta = foldl' makeMeta Map.empty . map words . lines
          where 
            makeMeta metaMap ([identifier] : direction : items)
              = Map.insert identifier (t (stringToDirection direction) $ map stringToItem items) metaMap
                where t = case identifier of 
                        '!' -> Agent
                        _   -> \dir items -> Professor dir $ map itemWithLast items
        
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


