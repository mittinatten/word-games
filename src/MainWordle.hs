module Main where

import Wordle
import WordList
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import Data.Char (toLower)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  allWords <- gameWords 5 5
  word <- randomWord' 5 5
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle allWords
