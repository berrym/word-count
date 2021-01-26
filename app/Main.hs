module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import System.IO
import WordCountLib

data Flags = Flags {
  filePath :: FilePath,
  countLines :: Bool,
  countWords :: Bool,
  countChars :: Bool
}

flags :: Parser Flags
flags = Flags
  <$> argument str (metavar "FILES.."
    <> help "File(s) to process")
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
  
main :: IO ()
main = count =<< execParser opts
  where
    opts = info (flags <**> helper)
      (fullDesc
      <> progDesc "Count words in a file."
      <> header "word-count")

count :: Flags -> IO ()
count (Flags filePath False False False) = do
  withFile filePath ReadMode $ \handle -> do
    putFileCount handle
count (Flags filePath countLines False False) = do
  withFile filePath ReadMode $ \handle -> do
    putLineCount handle
count (Flags filePath False countWords False) = do
  withFile filePath ReadMode $ \handle -> do
    putWordCount handle
count (Flags filePath False False countChars) = do
  withFile filePath ReadMode $ \handle -> do
    putCharCount handle
count (Flags filePath countLines countWords False) = do
  withFile filePath ReadMode $ \handle -> do
    putLineCount handle
    putWordCount handle
count (Flags filePath countLines False countChars) = do
  withFile filePath ReadMode $ \handle -> do
    putLineCount handle
    putCharCount handle
count (Flags filePath False countWords countChars) = do
  withFile filePath ReadMode $ \handle -> do
    putWordCount handle
    putCharCount handle
