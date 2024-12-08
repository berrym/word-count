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

-- | A record to store file statistics.
data FileStats = FileStats
  { lc :: Int, -- Line count
    wc :: Int, -- Word count
    bc :: Int -- File size in bytes
  }

-- | A record to keep states during counting.
data State = State
  { ls :: Int,
    ws :: Int,
    bs :: Int,
    wasSpace :: Int
  }

-- | Count line/words/bytes in a ByteString.
wordCount :: BS.ByteString -> FileStats
wordCount s = FileStats ls (ws + 1 - wasSpace) bs
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

-- | Read stats of a file and return the results in a FileStats record.
readFileStats :: FilePath -> IO FileStats
readFileStats fp = do
  s <- BS.readFile fp -- Read a file into a lazy ByteString.
  let fs = wordCount s -- Create a FileStats record.
  pure fs -- Return the FileStats record.
{-# INLINE readFileStats #-}
