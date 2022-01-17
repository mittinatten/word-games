{-# LANGUAGE LambdaCase #-}

module Wordle where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)

type WordList = [String]

data CharResult = Correct Char | InWord Char | NotInWord Char deriving (Ord, Eq)

data Puzzle = Puzzle String [[CharResult]]

isCorrect :: CharResult -> Bool
isCorrect (Correct _) = True
isCorrect _ = False

isInWord :: CharResult -> Bool
isInWord (InWord _) = True
isInWord _ = False

isNotInWord :: CharResult -> Bool
isNotInWord (NotInWord _) = True
isNotInWord _ = False

extractChar :: CharResult -> Char
extractChar (Correct c) = c
extractChar (InWord c) = c
extractChar (NotInWord c) = c

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word []

red :: String
red = "\ESC[31m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

magenta :: String
magenta = "\ESC[35m"

bold :: String
bold = "\ESC[1m"

underline :: String
underline = "\ESC[4m"

resetFormat :: String
resetFormat = "\ESC[0m"

instance Show CharResult where
  show (Correct c) = green ++ [c] ++ resetFormat
  show (InWord c) = magenta ++ [c] ++ resetFormat
  show (NotInWord c) = [c]

instance Show Puzzle where
  show (Puzzle _ results) =
    concat . reverse $ intersperse "\n" $ fmap mapper results
    where
      resultFolder line r = line ++ bold ++ show r
      mapper result = foldl resultFolder "" result

showLetters :: Puzzle -> String
showLetters (Puzzle word results@(lastResult : prevResults)) =
  let allGuessedChars = concat results
      allRawChars = fmap extractChar allGuessedChars
      inWord = extractChar <$> filter isInWord allGuessedChars
      notInWord = nub $ extractChar <$> filter isNotInWord allGuessedChars
      otherLetters = filter (`notElem` allRawChars) ['a' .. 'z']

      displayCorrect =
        foldr
          ( \rs correct ->
              let thisResult = mapCorrect rs
               in zipWith (\t c -> if t == '_' then c else t) thisResult correct
          )
          (mapCorrect lastResult)
          prevResults
        where
          mapCorrect =
            fmap
              ( \case
                  Correct c -> c
                  _ -> '_'
              )

      displayInWord = nub $ filter (\c -> count c word - count c displayCorrect > 0) inWord
   in "Correct position: " ++ green ++ displayCorrect ++ resetFormat ++ "\n"
        ++ "In word: "
        ++ yellow
        ++ displayInWord
        ++ resetFormat
        ++ "\n"
        ++ "Not in word: "
        ++ red
        ++ notInWord
        ++ resetFormat
        ++ "\n"
        ++ "Unused letters: "
        ++ otherLetters
showLetters (Puzzle _ []) = ""

guessWord :: Puzzle -> String -> Puzzle
guessWord (Puzzle word results) guess =
  Puzzle word (newResults : results)
  where
    zipper wordChar guessChar
      | wordChar == guessChar = Correct guessChar
      | guessChar `elem` word = InWord guessChar
      | otherwise = NotInWord guessChar
    -- these results can contain double counting of characters as both
    -- InWord and Correct
    zippedResults = zipWith zipper word (fmap toLower guess)

    -- remove double counting
    correctChars = (fmap extractChar . filter isCorrect) zippedResults

    folder (InWord c) crs =
      let countInWordSoFar = count c $ (fmap extractChar . filter isInWord) crs
          totalCorrectOfC = count c correctChars
          totalCInWord = count c word
       in if totalCInWord == totalCorrectOfC + countInWordSoFar
            then NotInWord c : crs
            else InWord c : crs
    folder cr crs = cr : crs

    newResults = foldr folder [] zippedResults

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word results) =
  when (length results >= 6) $
    do
      putStrLn "\nYou lose! 😬 "
      putStrLn $ "The word was: " ++ bold ++ green ++ word ++ resetFormat
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word (lastResult : _)) =
  when (length word == length correct) $
    do
      putStrLn "\nYou win! 🥳 "
      putStrLn $ "The word is: " ++ bold ++ green ++ word ++ resetFormat
      exitSuccess
  where
    correct = filter isCorrect lastResult
gameWin (Puzzle _ []) = do return ()

formatHeading :: String -> String
formatHeading t = underline ++ t ++ resetFormat

formatWarning :: String -> String
formatWarning w = bold ++ yellow ++ "[" ++ w ++ "]" ++ resetFormat

runGame :: Puzzle -> WordList -> IO ()
runGame puzzle@(Puzzle word results) wordList = forever $ do
  putStr $
    if not (null results)
      then formatHeading "Guesses" ++ "\n" ++ show puzzle ++ "\n"
      else ""
  gameWin puzzle
  gameOver puzzle

  let wordLength = length word

  putStr $
    if not (null results)
      then "\n" ++ formatHeading "Letters" ++ "\n" ++ showLetters puzzle ++ "\n"
      else ""
  putStr $ "\nGuess a " ++ show wordLength ++ "-letter word: " ++ bold
  guess <- getLine
  putStrLn resetFormat

  if wordLength /= length guess
    then putStrLn $ formatWarning ("Your guess must be a " ++ show wordLength ++ "-letter word")
    else
      if guess `notElem` wordList
        then putStrLn $ formatWarning "Your guess is not in the dictionary"
        else runGame (guessWord puzzle guess) wordList
