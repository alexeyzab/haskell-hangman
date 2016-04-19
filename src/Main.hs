module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed mistakes) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    ++ " Mistakes: " ++ (show mistakes)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map toNothing s) [] 0

toNothing :: a -> Maybe a
toNothing a = Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s mistakes) c =
  Puzzle word newFilledInSoFar (c : s) mistakes
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

incMistake :: Puzzle -> Puzzle
incMistake (Puzzle word filled guessed mistakes) = Puzzle word filled guessed (succ mistakes)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
   (_, True) -> do
     putStrLn "You alread guessed that\
               \ character, pick something else!"
     return puzzle
   (True, _) -> do
     putStrLn "This character was in the word,\
               \ filling in the word accordingly"
     return (fillInCharacter puzzle guess)
   (False, _) -> do
     putStrLn "This character wasn't in\
               \ the word, try again."
     return (fillInCharacter (incMistake puzzle) guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed mistakes) =
  if mistakes > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The  word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
   [c] -> handleGuess puzzle c >>= runGame
   _ -> putStrLn "Your guess must\
                  \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
