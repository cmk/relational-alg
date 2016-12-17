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
