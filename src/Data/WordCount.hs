{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

-- | Library code for word-count, count lines/words/bytes in files.
--     WordCountLib.hs
module Data.WordCount
  ( FileStats (..),
    readFileStats,
  )
where

import qualified Data.ByteString.Lazy as BS

-- | A record to keep the states of the counting.
data State = State
  { ls :: Int,
    ws :: Int,
    bs :: Int,
    wasSpace :: Int
  }

-- | Count line/words/bytes in a ByteString.
wordCount :: BS.ByteString -> (Int, Int, Int)
wordCount s = (ls, ws + 1 - wasSpace, bs)
  where
    State {..} = BS.foldl' go (State 0 0 0 1) s

    go State {..} c = State (ls + incLine) (ws + incWord) (bs + 1) isSpace
      where
        isSpace
          | c == 32 || c - 9 <= 4 = 1
          | otherwise = 0
        incLine
          | c == 10 = 1
          | otherwise = 0
        incWord = (1 - wasSpace) * isSpace
{-# INLINE wordCount #-}

-- | Get the first element of a three item tuple.
firstOf3 :: (a, b, c) -> a
firstOf3 (a, _, _) = a

-- | Get the second element of a three item tuple.
secondOf3 :: (a, b, c) -> b
secondOf3 (_, b, _) = b

-- | Get the third element of a three item tuple.
thirdOf3 :: (a, b, c) -> c
thirdOf3 (_, _, c) = c

-- | A record to store file statistics.
data FileStats = FileStats
  { lc :: Int, -- Line count
    wc :: Int, -- Word count
    bc :: Int -- File size in bytes
  }

-- | Read stats of a file and return the results in a FileStats record.
readFileStats :: FilePath -> IO FileStats
readFileStats fp = do
  contents <- BS.readFile fp -- Read a file into a lazy ByteString
  let counts = wordCount contents
  let l = firstOf3 counts
  let w = secondOf3 counts
  let b = thirdOf3 counts
  let fs = FileStats l w b -- Create a FileStat record
  pure fs -- Return the FileStat record
{-# INLINE readFileStats #-}
