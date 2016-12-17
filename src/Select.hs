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



