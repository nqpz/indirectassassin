{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Misc where

(.<) :: Integral n => n -> (a -> a) -> a -> a
(.<) 0 f v = v
(.<) n f v = (.<) (n - 1) f $ f v
infixr 9 .<

chunk :: Int -> [a] -> [[a]]
chunk n xs@(_ : xs') = let (ys, zs) = splitAt n xs
                        in ys : chunk n xs'
chunk _ [] = []

unchunk :: [[a]] -> [a]
unchunk = map head

splitSub :: Eq a => [a] -> [a] -> ([a], [a])
splitSub d = split' . chunk (length d)
  where split' (xs@(x : _) : xss) 
          | xs == d   = ([], unchunk (drop (length d - 1) xss))
          | otherwise = let (ys, zs) = split' xss
                        in (x : ys, zs)
        split' [] = ([], [])

anyelem :: Eq a => [a] -> [a] -> Bool
anyelem xs ys = (\y -> y `elem` xs) `any` ys

outM :: Monad m => [m a] -> m [a]
outM xs = out xs []
  where out [] ys = return ys
        out (x : xs) ys = x >>= \y -> out xs (y : ys)

-- outM :: (Monad m1, Monad m2) => m1 m2 a -> m2 m1 a


instance (Num t, Num t1) => Num (t, t1) where
  (x, y) + (x', y') = (x + x', y + y')
  (x, y) * (x', y') = (x * x', y * y')
  (x, y) - (x', y') = (x - x', y - y')
  negate (x, y) = (-x, y)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger n = (fromIntegral n, 0)

