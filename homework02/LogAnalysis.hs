module LogAnalysis where

import Log
  ( LogMessage (..),
    MessageTree (..),
    MessageType (Error, Info, Warning),
  )

main :: IO ()
main = do
  contents <- readFile "sample.log"
  print (whatWentWrong (parse contents))

parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I" : ts : rest) -> LogMessage Info (read ts) (unwords rest)
  ("W" : ts : rest) -> LogMessage Warning (read ts) (unwords rest)
  ("E" : err : ts : rest) -> LogMessage (Error (read err)) (read ts) (unwords rest)
  _ -> Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert newMsg Leaf = Node Leaf newMsg Leaf
insert newMsg@(LogMessage _ ts1 _) (Node left msg@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left msg (insert newMsg right)
  | otherwise = Node (insert newMsg left) msg right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

logFilter :: LogMessage -> Bool
logFilter (LogMessage (Error severity) _ _) = severity > 50
logFilter _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = [msg | LogMessage _ _ msg <- (inOrder . build . filter logFilter) logs]