{- | word-count: count lines/words/characters in files.
     Main.hs
-}
module Main (main) where

import Control.Monad
import Options.Applicative
import System.IO
import WordCountLib

{- | A data type for the command line parser. -}
data Flags = Flags
    { filePaths  :: [FilePath]  -- List of file paths to parse
    , countLines :: Bool        -- Boolean flag to count lines in files
    , countWords :: Bool        -- Boolean flag to count words in files
    , countChars :: Bool        -- Boolean flag to count characters in files
    }

{- | A parser built applicative style for optparse-applicative. -}
flags :: Parser Flags
flags = Flags
    <$> some (argument str (metavar "FILES.."
      <> help "File(s) to process"))
    <*> switch
      (long "countLines"
      <> short 'l'
      <> help "Count lines in a file")
    <*> switch
      (long "countWords"
      <> short 'w'
      <> help "Count words in a file")
    <*> switch
      (long "countChars"
      <> short 'c'
      <> help "Count characters in a file")

{- | Main function. -}
main :: IO ()
main = countFileStats =<< execParser opts
  where
    opts = info (flags <**> helper)
      (fullDesc
       <> progDesc "Count words in files."
       <> header "word-count")

{- | Recursively count a list of file paths for occurances of lines, words,
     and characters.
-}
countFileStats :: Flags -> IO ()

{- | Edge case: the empty FilePath list, return () -}
countFileStats (Flags [] _ _ _) = pure ()

{- | File(s) were given but no flags, run all counting functions. -}
countFileStats (Flags fP False False False) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putFileCounts contents
        countFileStats (Flags (drop 1 fP) False False False)

{- | countLines flag was given so run the count lines function. -}
countFileStats (Flags fP cL False False) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putLineCount contents
        countFileStats (Flags (drop 1 fP) cL False False)

{- | countWords flag was given so run the count words function. -}
countFileStats (Flags fP False cW False) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putWordCount contents
        countFileStats (Flags (drop 1 fP) False cW False)

{- | countChars flag was given so run the count characters function. -}
countFileStats (Flags fP False False cC) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putCharCount contents
        countFileStats (Flags (drop 1 fP) False False cC)

{- | Count lines and words in files. -}
countFileStats (Flags fP cL cW False) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putLineCount contents
        putWordCount contents
        countFileStats (Flags (drop 1 fP) cL cW False)

{- | Count lines and characters in files. -}
countFileStats (Flags fP cL False cC) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putLineCount contents
        putCharCount contents
        countFileStats (Flags (drop 1 fP) cL False cC)

{- | Count words and characters in files. -}
countFileStats (Flags fP False cW cC) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putWordCount contents
        putCharCount contents
        countFileStats (Flags (drop 1 fP) False cW cC)

{- | All flags were given, run all counting functions. -}
countFileStats (Flags fP cL cW cC) = do
    let file = head fP
    putHeader file
    withFile file ReadMode $ \handle -> do
        contents <- readFileContents handle
        putFileCounts contents
        countFileStats (Flags (drop 1 fP) cL cW cC)

{- | Print a header for the file being processed. -}
putHeader :: FilePath -> IO ()
putHeader file = do
    putStrLn file
    replicateM_ (length file) (putStr "=")
    putStr "\n"
