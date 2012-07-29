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

module IndirectAssassin.Misc where

-- Global
import Prelude hiding (Right, Left)

(.<) :: Integral n => n -> (a -> a) -> a -> a
(.<) 0 f v = v
(.<) n f v = (.<) (n - 1) f $ f v
infixr 9 .<

onPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
onPair f g (a, b) = (f a, g b)

anyelem :: Eq a => [a] -> [a] -> Bool
anyelem xs ys = (\y -> y `elem` xs) `any` ys

anytrue :: [a -> Bool] -> a -> Bool
anytrue fs x = (\f -> f x) `any` fs

outM :: Monad m => [m a] -> m [a]
outM xs = out xs []
  where out [] ys = return ys
        out (x : xs) ys = x >>= \y -> out xs (y : ys)

toUnit :: Monad m => [m a] -> m ()
toUnit xs = outM xs >> return ()
  
posrem :: Int -> Int -> Int
posrem n r = pos $ n `rem` r
  where pos n | n < 0 = r + n
              | otherwise = n

instance (Num t, Num t1) => Num (t, t1) where
  (x, y) + (x', y') = (x + x', y + y')
  (x, y) * (x', y') = (x * x', y * y')
  (x, y) - (x', y') = (x - x', y - y')
  negate (x, y) = (-x, y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromIntegral n, fromIntegral n)
