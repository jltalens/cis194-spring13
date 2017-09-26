module Golf where

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map snd $ filter f $ zip [0..] xs where
  f = \(i,v) -> (i > 0) && (i < length xs -1) && biggerThanNeigh i v
  biggerThanNeigh = \i v -> v > xs !!(i -1) && v > xs!!(i+1)
