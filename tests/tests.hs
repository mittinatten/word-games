module Main where

import Hangman
import Test.QuickCheck

alphabet :: [Char]
alphabet = ['a' .. 'z']

instance Arbitrary Puzzle where
  arbitrary = do
    -- Don't generate words with more than 15 characters,
    -- to avoid trouble generating wrong guesses below (15 is less than the length of the alphabet)
    word <- scale (`mod` 15) (listOf1 $ elements alphabet)
    return $ freshPuzzle word

genCorrectGuess :: Puzzle -> Gen Char
genCorrectGuess (Puzzle word _ _) = elements word

data CorrectGuess = CorrectGuess Puzzle Char deriving (Show, Eq)

instance Arbitrary CorrectGuess where
  arbitrary = do
    puzzle <- arbitrary
    guess <- genCorrectGuess puzzle
    return $ CorrectGuess puzzle guess

genWrongGuess :: Puzzle -> Gen Char
genWrongGuess (Puzzle word _ _) =
  elements (filter (`notElem` word) alphabet)

data WrongGuess = WrongGuess Puzzle Char deriving (Show, Eq)

instance Arbitrary WrongGuess where
  arbitrary = do
    puzzle <- arbitrary
    guess <- genWrongGuess puzzle
    return $ WrongGuess puzzle guess

data AnyGuess = AnyGuess Puzzle Char deriving (Show, Eq)

instance Arbitrary AnyGuess where
  arbitrary = do
    puzzle <- arbitrary
    guess <- oneof [genWrongGuess puzzle, genCorrectGuess puzzle]
    return $ AnyGuess puzzle guess

prop_fillingInCorrectCharAddsJust :: CorrectGuess -> Bool
prop_fillingInCorrectCharAddsJust (CorrectGuess puzzle guess) =
  let (Puzzle _ discovered guesses) = fillInCharacter puzzle guess
   in elem guess guesses
        && elem (Just guess) discovered

prop_fillingInCharacterIsIdempotent :: AnyGuess -> Bool
prop_fillingInCharacterIsIdempotent (AnyGuess puzzle guess) =
  let puzzle' = fillInCharacter puzzle guess
      puzzle'' = fillInCharacter puzzle' guess
   in puzzle' == puzzle''

prop_guessingIsIdemPotent :: AnyGuess -> Bool
prop_guessingIsIdemPotent (AnyGuess puzzle guess) =
  let (puzzle', _) = handleGuess puzzle guess
      (puzzle'', _) = handleGuess puzzle' guess
   in puzzle' == puzzle''

prop_guessingWrongCharOnlyAddsToListOfGuesses :: WrongGuess -> Bool
prop_guessingWrongCharOnlyAddsToListOfGuesses (WrongGuess puzzle@(Puzzle _ discovered _) guess) =
  let (Puzzle _ discovered' guesses', _) = handleGuess puzzle guess
   in guess `elem` guesses'
        && discovered == discovered'

-- what else to test?

main :: IO ()
main = do
  quickCheck prop_fillingInCorrectCharAddsJust
  quickCheck prop_fillingInCharacterIsIdempotent
  quickCheck prop_guessingIsIdemPotent
  quickCheck prop_guessingWrongCharOnlyAddsToListOfGuesses
