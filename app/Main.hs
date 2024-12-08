{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- | word-count: count lines/words/bytes in files.
--     Main.hs
module Main (main) where

import Data.WordCount
import Options.Applicative

-- | A data type for the command line parser.
data Flags = Flags
  { _filePaths :: [FilePath], -- List of file paths to parse
    _countLines :: Bool, -- Boolean flag to count lines in files
    _countWords :: Bool, -- Boolean flag to count words in files
    _countBytes :: Bool -- Boolean flag to count bytes in files
  }

-- | A parser built applicative style using optparse-applicative.
flags :: Parser Flags
flags =
  Flags
    <$> some
      ( argument
          str
          ( metavar "FILES.."
              <> help "File(s) to process"
          )
      )
    <*> switch
      ( long "countLines"
          <> short 'l'
          <> help "Count lines in a file"
      )
    <*> switch
      ( long "countWords"
          <> short 'w'
          <> help "Count words in a file"
      )
    <*> switch
      ( long "countBytes"
          <> short 'b'
          <> help "Count file size in bytes"
      )

-- | Main function.
main :: IO ()
main = putFileStats =<< execParser opts
  where
    opts =
      info
        (flags <**> helper)
        ( fullDesc
            <> progDesc "Count lines/words/bytes in file(s)."
            <> header "word-count"
        )

-- | Print the line cout, word count, character count and size of file(s).
putFileStats :: Flags -> IO ()

-- | Edge case: the empty FilePath list, return ()
putFileStats (Flags [] _ _ _) = pure ()
-- \| File(s) were given but no flags, print all counts in file(s).
putFileStats (Flags fP False False False) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (lc fs) ++ " " ++ show (wc fs) ++ " " ++ show (bc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) False False False)

-- \| countLines flag was given so print the line count in file(s).
putFileStats (Flags fP cL False False) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (lc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) cL False False)

-- \| countWords flag was given so print the word count in file(s).
putFileStats (Flags fP False cW False) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (wc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) False cW False)

-- \| countBytes flag was given so print the byte count in file(s).
putFileStats (Flags fP False False cB) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (bc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) False False cB)

-- \| Print the line and word counts in file(s).
putFileStats (Flags fP cL cW False) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (lc fs) ++ " " ++ show (wc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) cL cW False)

-- \| Print the line count and byte size of file(s).
putFileStats (Flags fP cL False cB) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (lc fs) ++ " " ++ show (bc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) cL False cB)

-- \| Print the word count and byte size of file(s).
putFileStats (Flags fP False cW cB) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (wc fs) ++ " " ++ show (bc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) False cW cB)

-- \| All flags were given.
putFileStats (Flags fP cL cW cB) = do
  let path = head fP
  fs <- readFileStats path
  putStrLn (" " ++ show (lc fs) ++ " " ++ show (wc fs) ++ " " ++ show (bc fs) ++ " " ++ path)
  putFileStats (Flags (drop 1 fP) cL cW cB)
