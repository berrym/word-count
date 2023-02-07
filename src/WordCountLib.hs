{- | Library code for word-count, count lines/words/characters/bytes in files.
     WordCountLib.hs
-}
module WordCountLib
    ( readFileStats
    , lc'
    , wc'
    , cc'
    , bc'
    , putByteCount
    , putLineCount
    , putWordCount
    , putCharCount
    , putFileCounts
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Lens.Micro.Platform
import qualified System.IO as IO

{- | A tuple to store file statistics. -}
type FileStats = ( Int      -- Line count
                 , Int      -- Word count
                 , Int      -- Character count
                 , Integer  -- File size in bytes
                 )

{- | Read stats of a file and return the results in a FileStats tuple. -}
readFileStats :: IO.Handle -> IO FileStats
readFileStats handle = do
    bc <- IO.hFileSize handle          -- Get the size of a file in bytes  
    contents <- B.hGetContents handle  -- Read contents of a file as ByteString
    let lc = lineCount contents        -- Get the line count of contents
    let wc = wordCount contents        -- Get the word count of contents
    let cc = charCount contents        -- Get the character count of contents
    let fs = (lc, wc, cc, bc)          -- create a FileStat tuple
    pure fs                            -- return the FileStat tuple

{- | Return the line count in a FileStats tuple using a lens. -}
lc' :: FileStats -> Int
lc' fileStats = fileStats ^. _1

{- | Return the word count in a FileStats tuple using a lens. -}
wc' :: FileStats -> Int
wc' fileStats = fileStats ^. _2

{- | Return the character count in a FileStats tuple using a lens. -}
cc' :: FileStats -> Int
cc' fileStats = fileStats ^. _3

{- | Return the file size in bytes in a FileStats tuple using a lens. -}
bc' :: FileStats -> Integer
bc' fileStats = fileStats ^. _4

{- | Given a ByteString count the number of lines in it. -}
lineCount :: B.ByteString -> Int
lineCount contents = (length . BC.lines) contents

{- | Given a ByteString count the number of words in it . -}
wordCount :: B.ByteString -> Int
wordCount contents = (length . BC.words) contents

{- | Given a ByteString count the number of characters in it. -}
charCount :: B.ByteString -> Int
charCount contents = B.length contents

{- | Print the results of lineCount. -}
putLineCount :: Int -> IO ()
putLineCount lc = putStrLn ("Lines:\t" ++ (show lc))

{- | Print the results of wordCount. -}
putWordCount :: Int -> IO ()
putWordCount wc = putStrLn ("Words:\t" ++ (show wc))

{- | Print the results of charCount. -}
putCharCount :: Int -> IO ()
putCharCount cc = putStrLn ("Chars:\t" ++ (show cc))

{- | Print the file size in bytes. -}
putByteCount :: Integer -> IO ()
putByteCount bc = putStrLn ("Bytes:\t" ++ (show bc))

{- | Print the results of all the counting functions. -}
putFileCounts :: FileStats -> IO ()
putFileCounts (lc, wc, cc, bc) = do
    putLineCount lc
    putWordCount wc
    putCharCount cc
    putByteCount bc
