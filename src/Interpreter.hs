module Interpreter where

import Select.Expression
import Select.Relation
import Table


-- instance Functor Expression
-- fmap :: (variable -> Value) -> Expression variable -> Expression Value

evalExpression :: (variable -> Value) -> Expression variable -> Value
evalExpression = undefined


evalRelation :: Relation scope variable table -> IO (Table)
evalRelation = undefined






