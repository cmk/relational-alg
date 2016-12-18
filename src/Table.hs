module Table where

import System.IO
import Data.Csv
import Data.Text

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Value = BoolValue Bool | IntValue Int | RealValue Double | StringValue String deriving Show

data ColName = ColName String deriving Show

type Row = [(ColName, Value)]
type Table = [Row]


--TODO: use utf8 to convert ByteString -> Text 

type Person = (Integer, BL.ByteString, BL.ByteString, Integer, Double, BL.ByteString)

personStats :: BL.ByteString -> Either String (V.Vector Person)
personStats = decode HasHeader

getVec :: IO ()
getVec  = do
  csvData <- BL.readFile "data/people.csv"
  let vec = personStats csvData
  print vec

orderId :: ColName
orderId = ColName "order_id"

customerId :: ColName
customerId = ColName "customer_id"

customerName :: ColName
customerName = ColName "customer_name"

customers :: Table
customers = [
   [(customerId, IntValue 1), (customerName, StringValue "Alfred")]
  ,[(customerId, IntValue 2), (customerName, StringValue "Ana")]
  ,[(customerId, IntValue 3), (customerName, StringValue "Antonio")]]

orders :: Table
orders = [
   [(orderId, IntValue 10308), (customerId, IntValue 2)]
  ,[(orderId, IntValue 10309), (customerId, IntValue 1)]
  ,[(orderId, IntValue 10310), (customerId, IntValue 3)]
  ,[(orderId, IntValue 10311), (customerId, IntValue 2)]]
        
