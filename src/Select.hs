module Select
  ( Select(..)
  , SelectIdentifier
  , execute
  ) where

import Select.Relation
import Select.Expression
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

-- Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)
  deriving (Eq,Show)

-- Identifier for SELECT specialized to our toy CSV backend
type SelectIdentifier = Select String String FilePath

-- Toy CSV backend. This executes the select statement to create a new CSV file
execute
  :: SelectIdentifier
  -> FilePath -- output
  -> IO ()
execute (SELECT (TABLE input)) output = do
  csvData <- BL.readFile input
  BL.writeFile output csvData

type Person = (Integer, BL.ByteString, BL.ByteString, Integer, Double, BL.ByteString)

personStats :: BL.ByteString -> Either String (V.Vector Person)
personStats = decode HasHeader

getVec :: IO ()
getVec  = do
  csvData <- BL.readFile "data/people.csv"
  let vec = personStats csvData
  print vec

-- evaluate (SumNode op left right) = 
--     let lft = evaluate left
--         rgt = evaluate right
--     in
--         case op of
--           Plus  -> lft + rgt
--           Minus -> lft - rgt
-- selectTable = SELECT (TABLE "data/people.csv")

--decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (V.Vector (V.Vector BL.ByteString))

--execute  (SELECT (TABLE "data/people.csv")) "data/output_name.csv"
