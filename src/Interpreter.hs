module Interpreter where

import Control.Monad.Reader

import Select.Expression
import Select.Relation
import Table


evalExpression :: Expression variable -> Reader Row (Maybe ColName, Value)
evalExpression = undefined

evalRelation :: Relation scope variable table -> IO (Table)
evalRelation = undefined






