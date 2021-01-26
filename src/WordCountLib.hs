module WordCountLib
    ( putLineCount,
      putWordCount,
      putCharCount,
      putFileCount,
    ) where

import System.IO

lineCount :: String -> Int
lineCount contents = length (lines contents)

wordCount :: String -> Int
wordCount contents = length (words contents)

charCount :: String -> Int
charCount contents = length contents

putCount :: Handle -> String -> (String -> Int) -> IO ()
putCount handle strMsg func = do
  hSeek handle AbsoluteSeek 0
  contents <- readContents handle
  putStrLn(strMsg ++ show (func contents))

putLineCount :: Handle -> IO ()
putLineCount handle = do putCount handle "Lines:\t" lineCount

putWordCount :: Handle -> IO ()
putWordCount handle = do putCount handle "Words:\t" wordCount

putCharCount :: Handle -> IO ()
putCharCount handle = do putCount handle "Chars:\t" charCount

putFileCount :: Handle -> IO ()
putFileCount handle = do
  putLineCount handle
  putWordCount handle
  putCharCount handle

readContents :: Handle -> IO String
readContents handle = do
  contents <- hGetLine handle
  inEOF <- hIsEOF handle
  if not inEOF
    then do readContents handle
    else return contents
