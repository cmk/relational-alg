module Table where

data Value = BoolValue Bool | IntValue Int | RealValue Double | StringValue String deriving (Read,Show,Eq,Ord)

type Row = [(String, Value)]
type Table = [Row]

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
        
