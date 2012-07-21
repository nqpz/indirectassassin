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
