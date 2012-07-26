{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

outM :: Monad m => [m a] -> m [a]
outM xs = out xs []
  where out [] ys = return ys
        out (x : xs) ys = x >>= \y -> out xs (y : ys)

posrem :: Int -> Int -> Int
posrem n r = pos $ n `rem` r
  where pos n | n < 0 = 4 + n
              | otherwise = n

instance (Num t, Num t1) => Num (t, t1) where
  (x, y) + (x', y') = (x + x', y + y')
  (x, y) * (x', y') = (x * x', y * y')
  (x, y) - (x', y') = (x - x', y - y')
  negate (x, y) = (-x, y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromIntegral n, fromIntegral n)

