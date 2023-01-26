{- | Library code for word-count, count words/lines/characters in files.
     WordCountLib.hs
-}
module WordCountLib
    (
      readFileContents
    , putLineCount
    , putWordCount
    , putCharCount
    , putFileCounts
    ) where

import System.IO

{- | Read the contents of a file and return it as an IO String. -}
readFileContents :: Handle -> IO String
readFileContents handle = do
    contents <- hGetContents handle
    pure contents

{- | Given a string count the number of lines in it. -}
lineCount :: String -> Int
lineCount contents = (length . lines) contents

{- | Given a string count the number of words in it . -}
wordCount :: String -> Int
wordCount contents = (length . words) contents

{- | Given a string count the number of characters in it. -}
charCount :: String -> Int
charCount contents = length contents

{- | Given one of the counting functions print a message and it's results. -}
putCount :: String -> String -> (String -> Int) -> IO ()
putCount contents strMsg func = do
    putStrLn(strMsg ++ show (func contents))
    putStr "\n"

{- | Print the results of lineCount. -}
putLineCount :: String -> IO ()
putLineCount contents = putCount contents "Lines:\t" lineCount

{- | Print the results of wordCount. -}
putWordCount :: String -> IO ()
putWordCount contents = putCount contents "Words:\t" wordCount

{- | Print the results of charCount. -}
putCharCount :: String -> IO ()
putCharCount contents = putCount contents "Chars:\t" charCount

{- | Print the results of all the counting functions. -}
putFileCounts :: String -> IO ()
putFileCounts contents = do
    putLineCount contents
    putWordCount contents
    putCharCount contents
