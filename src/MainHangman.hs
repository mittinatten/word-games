module Main where

import Hangman
import WordList
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import Data.Char (toLower)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord' 5 8
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
