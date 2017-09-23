module Golf where

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldl f [] (zip [0..] xs) where
  f = \acc (i,v) -> if i <= length xs then acc ++ []
