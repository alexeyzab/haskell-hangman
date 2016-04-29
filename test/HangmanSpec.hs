module HangmanSpec where

import Test.Hspec

import Hangman

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "minWordLength" $ do

    it "specifies the minimum word length" $ do
      minWordLength `shouldBe` 5

  describe "maxWordLength" $ do

    it "specifies the maximum word length" $ do
      maxWordLength `shouldBe` 9

  describe "charInWord" $ do

    it "returns True if it appears in word" $ do
      charInWord (Puzzle "ambulance" [] [] 0) 'l' `shouldBe` True

    it "returns False if it doesn't" $ do
      charInWord (Puzzle "ambulance" [] [] 0) 'x' `shouldBe` False

  describe "alreadyGuessed" $ do

    it "returns True if you already guessed this char" $ do
      alreadyGuessed (Puzzle "ambulance" [] ['a'] 0) 'a' `shouldBe` True

    it "returns False if you didn't" $ do
      alreadyGuessed (Puzzle "ambulance" [] ['a'] 0) 'x' `shouldBe` False

  describe "renderPuzzleChar" $ do
    
    it "renders puzzle chars" $ do
      renderPuzzleChar (Just 'k') `shouldBe` ('k')
      
    it "renders _ when given Nothing" $ do
      renderPuzzleChar Nothing `shouldBe` '_'
  
  describe "fillInCharacter" $ do

    it "fills in a character when possible" $ do
      fillInCharacter (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1) 'b' `shouldBe` (Puzzle "ambulance" [Just 'a', Just 'm', Just 'b', Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "bamo" 1)
      
    it "doesn't fill a character otherwise" $ do
      fillInCharacter (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1) 'z' `shouldBe` (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "zamo" 1)

  describe "handleGuess" $ do

    it "returns back the same Puzzle if you already guessed this character" $ do
      handleGuess (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1) 'a' `shouldReturn` (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1)

    it "returns back the Puzzle with the correct character filled in" $ do
      handleGuess (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1) 'b' `shouldReturn` (Puzzle "ambulance" [Just 'a', Just 'm', Just 'b', Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "bamo" 1)

    it "returns the puzzle without filling in the character if you guessed wrong" $ do
      handleGuess (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "amo" 1) 'z' `shouldReturn` (Puzzle "ambulance" [Just 'a', Just 'm', Nothing, Nothing, Nothing, Just 'a', Nothing, Nothing, Nothing] "zamo" 2)

