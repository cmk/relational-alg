module Main (main) where

import Select
import Select.Expression
import Select.Relation
import System.IO
import Test.Hspec
import Table

   
main :: IO ()
main = hspec $

  describe "execute" $ do

    it "can select a table" $ do
      execute selectTable "output_people.csv"
      "output_people.csv" `shouldHaveSameContentAs` "data/people.csv"

    it "performs filtering and comparisons correctly" $ do
      execute selectName "output_name.csv"
      "output_name.csv" `shouldHaveSameContentAs` "data/expected_output_name.csv"

    it "performs joins correctly" $ do
      execute selectJoin "output_join.csv"
      "output_join.csv" `shouldHaveSameContentAs` "data/expected_output_join.csv"

selectTable :: SelectIdentifier
selectTable = SELECT (TABLE "data/people.csv")

selectName :: SelectIdentifier
selectName =
  SELECT $ [Column "first_name" `AS` "name"]
    `FROM` TABLE "data/people.csv" `WHERE` (Column "age" `Gte` LiteralInt 40)

selectJoin :: SelectIdentifier
selectJoin = SELECT $
  [ Column "orders.order_id" `AS` "order_id"
  , Column "customers.customer_name" `AS` "customer_name"
  ] `FROM`
  ( TABLE "data/orders.csv" `AS` "orders"
    `INNER_JOIN_ON`
    TABLE "data/customers.csv" `AS` "customers"
  ) (Column ("orders","customer_id") `Equ` Column ("customers","customer_id"))

shouldHaveSameContentAs :: FilePath -> FilePath -> Expectation
file1 `shouldHaveSameContentAs` file2 =
  withFile file1 ReadMode $ \handle1 ->
    withFile file2 ReadMode $ \handle2 -> do
      contents1 <- hGetContents handle1
      contents2 <- hGetContents handle2
      contents1 `shouldBe` contents2
