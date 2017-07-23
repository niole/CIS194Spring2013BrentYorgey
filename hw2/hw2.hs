module LogAnalysis where

import Log
import Data.List.Split as T

stringToInt :: String -> Int
stringToInt s = read s :: Int

getMessageType :: String -> String -> MessageType
getMessageType a b = case a of
                        "I" -> Info
                        "E" -> Error (stringToInt b)
                        "W" -> Warning
                        _ -> Error (-1)

getLogMessage :: String -> String -> [String] -> LogMessage
getLogMessage a b (c:cs) = case (getMessageType a b) of
                                Error (-1) -> Unknown (joinWords (a : b : c : cs ))
                                Error n -> LogMessage (Error n) (stringToInt c) (joinWords cs)
                                Info -> LogMessage Info (stringToInt b) (joinWords (c : cs))
                                Warning -> LogMessage Warning (stringToInt b) (joinWords (c : cs))

joinWords :: [String] -> String
joinWords (w : ws) = foldl (\a -> \b -> a ++ " " ++ b) (w) ws

parseMessage :: String -> LogMessage
parseMessage l = case (T.splitOn " " l) of
                        (a : b : []) -> Unknown l
                        (a : b : cs) -> getLogMessage a b cs
                        _ -> Unknown l

parse :: String -> [LogMessage]
parse s = parseMessage <$> (T.splitOn "\n" s)

--inserts a LogMessage into the tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert (LogMessage mType ts content) (Node a (LogMessage c ts2 d) b)
        | ts < ts2 = Node (insert (LogMessage mType ts content) a) (LogMessage c ts2 d) b
        | ts > ts2 = Node a (LogMessage c ts2 d) (insert (LogMessage mType ts content) b)

--builds a message tree from a list of nodes
build :: [LogMessage] -> MessageTree
build lms = foldl (\tree -> \node -> insert node tree) Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node Leaf m t) = m : inOrder t
inOrder (Node t m Leaf) = inOrder t ++ [m]
inOrder (Node x m y) = (inOrder x) ++ [m] ++ (inOrder y)

isBadError :: MessageType -> Bool
isBadError (Error n) = n > 50
isBadError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = (\(LogMessage _ _ c) -> c) <$> (filter (\(LogMessage mType _ _) -> isBadError mType) $ inOrder (build lms))
