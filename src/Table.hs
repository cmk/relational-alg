module Table where

import Control.Monad
import Data.List (nub)

import System.IO
import Data.Csv
import qualified Data.ByteString.Lazy as BL



data Value = BoolValue Bool | IntValue Int | RealValue Double | StringValue String deriving (Read,Show,Eq,Ord)


getInt :: Value -> Int
getInt (IntValue i) = i

getBool :: Value -> Bool
getBool (BoolValue b) = b

getReal :: Value -> Double
getReal (RealValue r) = r

getString :: Value -> String
getString (StringValue s) = s

type Row = [(String, Value)]
type Table = [Row]

-- temporary convenience tables for testing until parseTable is implemented.
customers :: Table
customers = [
   [("customerId", IntValue 1), ("customerName", StringValue "Alfred")]
  ,[("customerId", IntValue 2), ("customerName", StringValue "Ana")]
  ,[("customerId", IntValue 3), ("customerName", StringValue "Antonio")]]

orders :: Table
orders = [
   [("orderId", IntValue 10308), ("customerId", IntValue 2)]
  ,[("orderId", IntValue 10309), ("customerId", IntValue 1)]
  ,[("orderId", IntValue 10310), ("customerId", IntValue 3)]
  ,[("orderId", IntValue 10311), ("customerId", IntValue 2)]]
        
readTable :: FilePath -> IO (Table)
readTable filepath = return customers

writeTable :: filepath -> table -> IO ()
writeTable = undefined

---

selectOp :: [String] -> Table -> Table
selectOp colNames = map filterRow where
  filterRow row = row >>= (\cell -> if elem (fst cell) colNames then [cell] else [])

whereOp :: (Row -> Bool) -> Table -> Table
whereOp f tab = tab >>= (\row -> if f row then [row] else [])

unionOp :: Table -> Table -> Table
unionOp tab1 tab2 = nub (tab1 ++ tab2)

innerJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
innerJoin f ls rs = filter (uncurry f) (liftM2 (,) ls rs)
