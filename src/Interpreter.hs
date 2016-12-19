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

evaluateE (LiteralBool x) = return $ BoolValue x
evaluateE (LiteralString x) = return $ StringValue x
evaluateE (LiteralInt x) = return $ IntValue x
evaluateE (LiteralReal x) = return $ RealValue x

evaluateE (Column str) = do
  symTab <- get
  case M.lookup (show str) symTab of
    Just v -> return v
    Nothing -> error $ "Undefined variable"

evaluateE (Neg exp) = do
  ex <- evaluateE exp
  case ex of
    IntValue e -> return $ IntValue (-e)
    RealValue e -> return $ RealValue (-e)
    _ -> error $ "Types not supported by Neg"

evaluateE (Not exp) = do
  ex <- evaluateE exp
  case ex of
    BoolValue e -> return $ BoolValue (not e)
    _ -> error $ "Types not supported by Not"

evaluateE (And left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (BoolValue l, BoolValue r) -> return $ BoolValue (l && r)
    _ -> error $ "Types not supported by Add"

evaluateE (Or left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (BoolValue l, BoolValue r) -> return $ BoolValue (l || r)
    _ -> error $ "Types not supported by Add"
    
evaluateE (Equ left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft == rgt)

evaluateE (Neq left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft /= rgt)
  
evaluateE (Gt left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft > rgt)
  
evaluateE (Gte left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft >= rgt)

evaluateE (Lt left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft < rgt)

evaluateE (Lte left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  return $ BoolValue (lft <= rgt)
  
evaluateE (Add left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (IntValue l, IntValue r) -> return $ IntValue (l + r)
    (RealValue l, RealValue r) -> return $ RealValue (l + r)
    _ -> error $ "Types not supported by Add"

evaluateE (Sub left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (IntValue l, IntValue r) -> return $ IntValue (l - r)
    (RealValue l, RealValue r) -> return $ RealValue (l - r)
    _ -> error $ "Types not supported by Sub"

evaluateE (Mul left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (IntValue l, IntValue r) -> return $ IntValue (l * r)
    (RealValue l, RealValue r) -> return $ RealValue (l * r)
    _ -> error $ "Types not supported by Mul"
    
evaluateE (Div left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (IntValue l, IntValue r) -> return $ IntValue (l `div` r)
    _ -> error $ "Types not supported by Div"

evaluateE (Mod left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (IntValue l, IntValue r) -> return $ IntValue (l `mod` r)
    _ -> error $ "Types not supported by Mod"

---

evaluateR :: Relation scope variable table -> IO (Table)
evaluateR = undefined






