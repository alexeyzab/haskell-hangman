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
