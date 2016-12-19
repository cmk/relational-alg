{-# LANGUAGE NoMonomorphismRestriction #-}
module Interpreter where

import Data.Char
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad (liftM, ap, guard)
import Control.Monad.Trans.Maybe

import Select.Expression
import Select.Relation
import Table

type SymTab = M.Map String Value
type Evaluator a = MaybeT (State SymTab) a


checkNum :: (Value,Value) -> Bool
checkNum e = 
  case e of
    (IntValue l, IntValue r) -> True
    (RealValue l, RealValue r) -> True
    _ -> False

checkInt :: (Value,Value) -> Bool
checkInt e = 
  case e of
    (IntValue l, IntValue r) -> True
    _ -> False

checkBool :: (Value,Value) -> Bool
checkBool e = 
  case e of
    (BoolValue l, BoolValue r) -> True
    _ -> False
    
evaluateE :: Expression a -> Evaluator Value
      
evaluateE (LiteralInt x) = return $ IntValue x
evaluateE (LiteralReal x) = return $ RealValue x

-- evaluateE (Column x) =  do
--     symTab <- get
--     guard $ M.member x symTab
--     case M.lookup x symTab of
--       Just v -> return v

evaluateE (Add left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    guard $ checkNum (lft,rgt)
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l + r)
      (RealValue l, RealValue r) -> return $ RealValue (l + r)

evaluateE (Sub left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    guard $ checkNum (lft,rgt)
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l - r)
      (RealValue l, RealValue r) -> return $ RealValue (l - r)

evaluateE (Mul left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    guard $ checkNum (lft,rgt)
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l * r)
      (RealValue l, RealValue r) -> return $ RealValue (l * r)

evaluateE (Div left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    guard $ checkInt (lft,rgt)
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l `div` r)

evaluateE (Mod left right) = do
    lft <- evaluateE left
    rgt <- evaluateE right
    guard $ checkInt (lft,rgt)
    case (lft,rgt) of
      (IntValue l, IntValue r) -> return $ IntValue (l `mod` r)
      
---

evaluateR :: Relation scope variable table -> IO (Table)
evaluateR = undefined






