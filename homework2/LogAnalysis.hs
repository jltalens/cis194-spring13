{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case s of
  ('I':' ':xs) -> parseSimpleTypeMessage Info xs
  ('W':' ':xs) -> parseSimpleTypeMessage Warning xs
  ('E':' ':xs) -> parseErrorMessage xs
  _ -> Unknown s

parseSimpleTypeMessage :: MessageType -> String -> LogMessage
parseSimpleTypeMessage t s = LogMessage t timestamp rest where
  timestamp = read (extractNextBlock s) :: Int
  rest = dropTillText 1 s

extractNextBlock :: String -> String
extractNextBlock s = unwords $ take 1 $ words s

dropTillText :: Int -> String -> String
dropTillText n s = unwords $ drop n $ words s

parseErrorMessage :: String -> LogMessage
parseErrorMessage s = LogMessage (Error errorLevel) timestamp rest where
  errorLevel = read (extractNextBlock s) :: Int
  timestamp = read (extractNextBlock $ dropTillText 1 s) :: Int
  rest = dropTillText 2 s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t1 _) (Node left m1@(LogMessage _ t2 _) right)
  | t1 <= t2 = Node (insert m left) m1 right
  | t1 > t2 = Node left m1 (insert m right)


build :: [LogMessage] -> MessageTree
build xs = buildInner xs Leaf

buildInner :: [LogMessage] -> MessageTree -> MessageTree
buildInner [] t = t
buildInner (x:xs) t = buildInner xs (insert x t)


inOrder :: MessageTree -> [LogMessage]
inOrder t = inOrderInner t []

inOrderInner :: MessageTree -> [LogMessage] -> [LogMessage]
inOrderInner Leaf xs = xs
inOrderInner (Node l v r) xs = inOrderInner l (inOrderInner r $ xs ++ [v])
