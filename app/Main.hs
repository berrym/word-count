{-# LANGUAGE OverloadedStrings #-}

{- | word-count: count lines/words/characters/bytes in files.
     Main.hs
-}
module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Control.Monad
import Options.Applicative
import qualified System.IO as IO
import WordCountLib

{- | A data type for the command line parser. -}
data Flags = Flags
    { filePaths  :: [FilePath]  -- List of file paths to parse
    , countLines :: Bool        -- Boolean flag to count lines in files
    , countWords :: Bool        -- Boolean flag to count words in files
    , countChars :: Bool        -- Boolean flag to count characters in files
    , countBytes :: Bool        -- Boolean flag to count bytes in files
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
    <*> switch
      (long "countBytes"
      <> short 'b'
      <> help "Count file size in bytes")

{- | Main function. -}
main :: IO ()
main = handleFileStats =<< execParser opts
  where
    opts = info (flags <**> helper)
      (fullDesc
       <> progDesc "Count lines/words/characters/bytes in file(s)."
       <> header "word-count")

{- | Print the line cout, word count, character count and size of file(s). -}
handleFileStats :: Flags -> IO ()

{- | Edge case: the empty FilePath list, return () -}
handleFileStats (Flags [] _ _ _ _) = pure ()

{- | File(s) were given but no flags, print all counts in file(s). -}
handleFileStats (Flags fP False False False False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putFileStats fileStats
        handleFileStats (Flags (drop 1 fP) False False False False)

{- | countLines flag was given so print the line count in file(s). -}
handleFileStats (Flags fP cL False False False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL False False False)

{- | countWords flag was given so print the word count in file(s). -}
handleFileStats (Flags fP False cW False False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putWordCount (wc' fileStats)
        handleFileStats (Flags (drop 1 fP) False cW False False)

{- | countChars flag was given so print the character count in file(s). -}
handleFileStats (Flags fP False False cC False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putCharCount (wc' fileStats)
        handleFileStats (Flags (drop 1 fP) False False cC False)

{- | countBytes flag was given so print the byte count in file(s). -}
handleFileStats (Flags fP False False False cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putByteCount (bc' fileStats)
        handleFileStats(Flags (drop 1 fP) False False False cB)

{- | Print the line and word counts in file(s). -}
handleFileStats (Flags fP cL cW False False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        putWordCount (wc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL cW False False)

{- | Print the line and character count in file(s). -}
handleFileStats (Flags fP cL False cC False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        putCharCount (cc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL False cC False)

{- | Print the word and character count in file(s). -}
handleFileStats (Flags fP False cW cC False) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putWordCount (wc' fileStats)
        putCharCount (cc' fileStats)
        handleFileStats (Flags (drop 1 fP) False cW cC False)

{- | Print the line count and byte size of file(s). -}
handleFileStats (Flags fP cL False False cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL False False cB)

{- | Print the word count and byte size of file(s). -}
handleFileStats (Flags fP False cW False cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putWordCount (wc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) False cW False cB)

{- | Print the character count and byte size of file(s). -}
handleFileStats (Flags fP False False cC cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putCharCount (cc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) False False cC cB)

{- | Print the line, word, character count and byte size of file(s). -}
handleFileStats (Flags fP cL cW False cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        putWordCount (wc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL cW False cB)

{- | Print the line, character count and byte size of file(s). -}
handleFileStats (Flags fP cL False cC cB)= do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putLineCount (lc' fileStats)
        putCharCount (cc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) cL False cC cB)

{- | Print the word, character count and byte size of file(s). -}
handleFileStats (Flags fP False cW cC cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putWordCount (wc' fileStats)
        putCharCount (cc' fileStats)
        putByteCount (bc' fileStats)
        handleFileStats (Flags (drop 1 fP) False cW cC cB)

{- | All flags were given, print all counts in file(s). -}
handleFileStats (Flags fP cL cW cC cB) = do
    let file = head fP
    putHeader file
    IO.withBinaryFile file IO.ReadMode $ \handle -> do
        fileStats <- readFileStats handle
        putFileStats fileStats
        handleFileStats (Flags (drop 1 fP) cL cW cC cB)

{- | Print a header for the file being processed. -}
putHeader :: FilePath -> IO ()
putHeader file = do
    replicateM_ (length file) (B.hPut IO.stdout "=")
    B.hPut IO.stdout "\n"
    B.hPut IO.stdout ((BC.pack file) <> "\n")
    replicateM_ (length file) (B.hPut IO.stdout "=")
    B.hPut IO.stdout "\n"
