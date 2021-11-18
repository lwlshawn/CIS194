{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
-- Ex1: parses messages based on the initial tag
parseMessage :: String -> LogMessage
parseMessage msg
    | (tag == "I") = LogMessage Info (read n) ((unwords . drop 2 ) xs)
    | (tag == "W") = LogMessage Warning (read n) ((unwords . drop 2) xs)
    | (tag == "E") = LogMessage (Error (read n)) (read (xs !! 2)) ((unwords . drop 3) xs)
    | otherwise = Unknown $ unwords xs
    where
        xs = words msg
        [tag, n] = take 2 xs


-- given a string representing a set of log messages, parses each message
parse :: String -> [LogMessage]
parse lg = [parseMessage x|x <- lines lg]


-- Ex2: incomplete pattern matching because cases need not consider Unknown being in
-- the tree. Not sure how to properly mark this stylistically. Insertion to BST
insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node lt cm@(LogMessage _ cts _) rt)
    | ts < cts = Node (insert m lt) cm rt
    | otherwise = Node lt cm (insert m rt) --why is otherwise diff from >=?
insert (Unknown _) t = t
insert _ _ = error "passed wrong constructor"


-- Ex3: builds a BST from a list of LogMessages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (h:t) = insert h (build t)


-- Ex4: inOrder traversal of BST to return sorted list
inOrder :: MessageTree -> [LogMessage]
inOrder (Node lt m rt) = inOrder lt ++ [m] ++ inOrder rt
inOrder Leaf = []


-- Ex5: print list of messages with error >= 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lg = helper $ inOrder $ build lg
                    where
                        helper ((LogMessage (Error n) _ m):t)
                            | n >= 50 = m : helper t
                            | otherwise = helper t
                        helper (_:t) = helper t
                        helper [] = []


{-
!! indexes from 0, so 2 to pull the third element. "parse type error" was thrown by 
calling read on a string.
-}