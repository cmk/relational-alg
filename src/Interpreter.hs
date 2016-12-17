module Interpreter where

import Data.Text

import Select.Expression
import Select.Relation
import Table


inferType :: String -> Value
inferType "True" = BoolValue True
inferType "False" = BoolValue False
inferType x | elem '.' x = RealValue (read x :: )

--etc

-- instance Functor Expression
-- fmap :: (variable -> Value) -> Expression variable -> Expression Value
-- fmap inferType (LiteralBool True)

evalExpression :: (a -> Value) -> Expression a -> Value
evalExpression inferType exp = case exp of
  

evalRelation :: Relation scope variable table -> IO (Table)
evalRelation = undefined






