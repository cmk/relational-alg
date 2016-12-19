{-# LANGUAGE NoMonomorphismRestriction #-}

module Interpreter where

import Data.Char
import qualified Data.Map as M
import Control.Monad.State

import Select.Expression
import Select.Relation
import Table

type SymTab = M.Map String Value
type Evaluator a = State SymTab a


evaluateE :: Show a =>  Expression a -> Evaluator Value
      
evaluateE (LiteralInt x) = return $ IntValue x
evaluateE (LiteralReal x) = return $ RealValue x

evaluateE (Column str) = do
  symTab <- get
  case M.lookup (show str) symTab of
    Just v -> return v
    Nothing -> error $ "Undefined variable"

evaluateE (Add left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l + r)
      (RealValue l, RealValue r) -> return $ RealValue (l + r)

evaluateE (Sub left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l - r)
      (RealValue l, RealValue r) -> return $ RealValue (l - r)

evaluateE (Mul left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l * r)
      (RealValue l, RealValue r) -> return $ RealValue (l * r)

evaluateE (Div left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l `div` r)

evaluateE (Mod left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l `mod` r)
      
---

evaluateR :: Relation scope variable table -> IO (Table)
evaluateR = undefined






