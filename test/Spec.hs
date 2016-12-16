module Main (main) where

import Select
import Select.Expression
import Select.Relation
import System.IO
import Test.Hspec

main :: IO ()
main = hspec $

  describe "execute" $ do

    it "can select a table" $ do
      execute selectTable "output_people.csv"
      "output_people.csv" `shouldHaveSameContentAs` "data/people.csv"


selectTable :: SelectIdentifier
selectTable = SELECT (TABLE "data/people.csv")


shouldHaveSameContentAs :: FilePath -> FilePath -> Expectation
file1 `shouldHaveSameContentAs` file2 =
  withFile file1 ReadMode $ \handle1 ->
    withFile file2 ReadMode $ \handle2 -> do
      contents1 <- hGetContents handle1
      contents2 <- hGetContents handle2
      contents1 `shouldBe` contents2
