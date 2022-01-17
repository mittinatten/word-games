module WordList where

import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: Int -> Int -> IO WordList
gameWords minWordLength maxWordLength = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: Int -> Int -> IO String
randomWord' minWordLength maxWordLength =
  gameWords minWordLength maxWordLength >>= randomWord
