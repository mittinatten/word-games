module Hangman where

import Control.Monad (forever, when)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Eq)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse
      ' '
      ( fmap renderPuzzleChar discovered
      )
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word =
  let l = length word
   in Puzzle word (replicate l Nothing) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar newS
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
    newS = if (not (elem c s)) then c : s else s

type Message = String

handleGuess :: Puzzle -> Char -> (Puzzle, Message)
handleGuess puzzle guess =
  let msg = "Your guess was: " ++ [guess] ++ "\n"
   in case ( charInWord puzzle guess,
             alreadyGuessed puzzle guess
           ) of
        (_, True) ->
          (puzzle, msg ++ "You already guessed that character, pick something else!")
        (True, _) ->
          (fillInCharacter puzzle guess, msg ++ "This character was in the word!")
        (False, _) ->
          (fillInCharacter puzzle guess, msg ++ "This character wasn't in the word, try again.")

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledIn guessed) =
  when ((length guessed - length (filter isJust filledIn)) > 7) $ do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn $ "You win! The word is: " ++ word ++ "."
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] ->
      let (puzzle', msg) = handleGuess puzzle c
       in do
            putStrLn msg
            runGame puzzle'
    _ -> putStrLn "Your guess must be a single character"
