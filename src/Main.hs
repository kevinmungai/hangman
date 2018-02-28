{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad (forever,when)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


-- type synoym for the list of strings we will get from
-- data/dict.txt
type WordList = [String]


-- reading the file data/dict.txt and then putting the data
-- into a list of Strings
allWords :: IO WordList
allWords =
  do
    dict <- readFile "data/dict.txt"
    return (lines dict)


-- setting the lower & upper bounds of words that will be used
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9


-- taking the words from 'allWords' and then filtering them according
-- to the 'minWordLength' and 'maxWordLength'
gameWords :: IO WordList
gameWords =
  do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
           let l = length (w :: String)
           in l > minWordLength && l < maxWordLength


-- a random word selector using randomRIO to facilitate the word
-- choosing
randomWord :: WordList -> IO String
randomWord wl =
  do
    randomIndex <- randomRIO (10,144) -- so random
    return $ wl !! randomIndex

-- binding the result of gameWords to randomWord
randomWord' :: IO String
randomWord' = gameWords >>= randomWord


---------------------------------------------------
-- Puzzle
---------------------------------------------------
-- We need to find a way to hide the word from the player
-- while giving them the ability to see the length of the word and
-- the letters guessed so far
--------------------------------------------------------------
-- We will create a data type to support that
-- Puzzle (word we're trying to guess) (char filled in so far)
-- (letters guessed so fa)
data Puzzle = Puzzle String [Maybe Char] String
--                   test     correct     letters used

-- Writing Puzzle as instance of the Show typeclass
-- Why? -> So as to customize the look of output of Puzzle
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

-- a function that will take our puzzle word and turn it into a list
-- of Nothing. The first step in hiding the word from the player.
--
-- So the function will take a String and return a Puzzle
freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle xs (map (const Nothing) xs) []

-- check whether the guessed letter is part of the word
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

-- check whether the guessed letter is part of the already guessed list
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

-- renderPuzzleChar takes a Maybe and returns the Char in the Just
-- or '_' in case of Nothing
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = \case
  Nothing -> '_'
  Just c -> c


-- inserting the guessed letter into the correct place and adding it
-- to the guessed List
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
  where
    zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar



handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
  do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
         , alreadyGuessed puzzle guess) of
      (_, True) ->
        do
          putStrLn "You already guessed that\
                    \ character, pick something else!"
          return puzzle
      (True, _) ->
        do
          putStrLn "This character was in the word,\
                   \ filling in the word accordingly"
          return (fillInCharacter puzzle guess)
      (False, _) ->
        do
          putStrLn "This character wasn't in\
                   \ the word, try again."
          return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length guessed > 7) $
      do
       putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess



gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ ) =
  when (all isJust filledInSoFar) $
    do
      putStrLn "You win!"
      exitSuccess



runGame :: Puzzle -> IO ()
runGame puzzle = forever $
  do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must\
                    \ be a single character"

