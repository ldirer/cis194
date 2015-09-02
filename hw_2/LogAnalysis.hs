{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- charToMessageType :: Char -> MessageType
-- charToMessageType c = case c of
--                        'I' -> Info
--                        'W' -> Warning


-- TODO: It's unclear what would be the proper way to handle the Unknown case.

splitOnFirstSpace :: String -> (String, String)
splitOnFirstSpace = addToPart 0
    where addToPart :: Int -> String -> (String, String)
          addToPart _ [] = ("", "")
          addToPart 1 xs = ("", xs)
          addToPart 0 (x:xs) | x == ' ' = addToPart 1 xs
                             | otherwise = (x : fst (addToPart 0 xs), snd (addToPart 0 xs))
          addToPart _ _ = error "Index must be 0 or 1, splitting in 2 parts"
                                             

                    
parseTimestampContent :: String -> (TimeStamp, String)
-- No need for type annotation for read (i.e read s :: Int), the signature gives it.
parseTimestampContent s = (read (fst sSplit), snd sSplit) 
    where sSplit = splitOnFirstSpace s

parseMessage :: String -> LogMessage
parseMessage a = let (c, rest) = splitOnFirstSpace a in 
                     case c of
                       "I" -> parseEnd Info rest a
                       "W" -> parseEnd Warning rest a
                       "E" -> case reads errorLevel :: [(Int, String)] of 
                                [] -> Unknown a
                                [(n, _)] -> parseEnd (Error n) end a
                                _ -> error "Unexpected reads output!"
                           where (errorLevel, end) = splitOnFirstSpace rest 
                       _ -> Unknown a
                    
-- WARNING: Unhandled bad format when there's something else than a number as second word.
parseEnd :: MessageType -> String -> String -> LogMessage
parseEnd m s full = case splitOnFirstSpace s of
                      ("", _) -> Unknown full
                      (ts, body) -> case reads ts :: [(Int, String)] of 
                                      [] -> Unknown full
                                      [(n, _)] -> LogMessage m n body
                                      _ -> error "Unexpected reads output!"


testSplitOnFirstSpace :: IO()                    
testSplitOnFirstSpace = let s = "1 bou" in
                            print $ splitOnFirstSpace s


-- Now same thing without reinventing the wheel.
betterParseEnd :: MessageType -> [String] -> String -> LogMessage
betterParseEnd _ [] full = Unknown full
betterParseEnd m (timestamp:xs) full = case reads timestamp :: [(Int, String)] of
                                         [] -> Unknown full
                                         [(n, _)] -> LogMessage m n (unwords xs)
                                         _ -> error "Unexpected reads output!"


betterParseMessage :: String -> LogMessage
betterParseMessage a = case words a of 
                         [] -> Unknown a
                         [_] -> Unknown a
                         (x1:xtail@(x2:_)) -> case x1 of
                                                "I" -> betterParseEnd Info xtail a
                                                "W" -> betterParseEnd Warning xtail a
                                                "E" -> case reads x2 :: [(Int, String)] of 
                                                         [] -> Unknown a
                                                         [(n, _)] -> betterParseEnd (Error n) (tail xtail) a
                                                         _ -> error "Unexpected reads output!"
                                                _ -> Unknown a
             


-- parse a whole log file
myParse :: String -> [LogMessage]
myParse s = map betterParseMessage $ lines s

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node tl n@(LogMessage _ tsnode _) tr) | ts > tsnode = Node tl n (insert m tr)
                                                                  | ts < tsnode = Node (insert m tl) n tr


-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m $ build ms


-- Exercise 4
-- Assumes a sorted message Tree! Returns the list of log messages in the tree sorted by ts.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node tl m tr) = inOrder tl ++ [m] ++ inOrder tr


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map getBodyError ((inOrder . build) $ filter isBadError ms)


isBadError :: LogMessage -> Bool
isBadError (LogMessage (Error n) _ _) = n >= 50
isBadError _ = False


getBodyError :: LogMessage -> String
getBodyError (LogMessage (Error _) _ body) = body
getBodyError _ = error "getBodyError works only on Error messages."


main :: IO()
main = do
    -- testSplitOnFirstSpace
    print $ splitOnFirstSpace "I bou"
    print $ parseMessage "I bou"
    -- logMessages <- testParse myParse 100 "./sample.log"
    -- print logMessages
    -- print $ (inOrder . build) logMessages
    www <- testWhatWentWrong myParse whatWentWrong "./sample.log" 
    print www
    wwwFull <- testWhatWentWrong myParse whatWentWrong "./error.log" 
    print wwwFull

