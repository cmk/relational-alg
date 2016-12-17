module Interpreter where

import Select.Expression
import Select.Relation
import Table



inferType :: String -> Value
inferType "True" = BoolValue True
inferType "False" = BoolValue False
--etc

-- instance Functor Expression
-- fmap :: (variable -> Value) -> Expression variable -> Expression Value
-- fmap inferType (LiteralBool True)

evalExpression :: (variable -> Value) -> Expression variable -> Value
evalExpression inferType exp = case exp of
  

evalRelation :: Relation scope variable table -> IO (Table)
evalRelation = undefined






