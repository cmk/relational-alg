module Select
  ( Select(..)
  , SelectIdentifier
  , execute
  ) where

import Select.Relation
import Select.Expression
import Interpreter
import Table

-- Top level `Select` AST
newtype Select scope variable table
  = SELECT (Relation scope variable table)
  deriving (Eq,Show)

-- Identifier for SELECT specialized to our toy CSV backend
type SelectIdentifier = Select String String FilePath

-- Toy CSV backend. This executes the select statement to create a new CSV file
execute :: SelectIdentifier -> FilePath -> IO ()
execute (SELECT rel) filepath = do
  table <- evaluateR rel
  writeTable filepath table

