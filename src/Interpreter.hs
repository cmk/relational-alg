module Interpreter where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Typeable
import Data.List (nub)
import qualified Data.Map as M

import Select.Expression
import Select.Relation
import Table

---

                      
evaluateR :: RelationIdentifier -> IO (Table)

evaluateR (TABLE filepath) = readTable filepath

evaluateR (projection `FROM` relation) = do
  tab <- evaluateR relation
  let evalCell row (scope, state) = (scope, evalState state (M.fromList row))
  let opList = map (\(AS x scope) -> (scope,evaluateE x)) projection
  let evalRow opList row = map (evalCell row) opList
  return $ map (evalRow opList) tab                

evaluateR (relation `WHERE` condition) = do
  tab <- evaluateR relation
  let cond = evaluateE condition
  let whereOp predicate table = table >>= (\row -> if predicate row then [row] else [])
  return $ whereOp (\row -> getBool $ evalState cond (M.fromList row)) tab

evaluateR (rel1 `UNION` rel2) = do
  tab1 <- evaluateR rel1
  tab2 <- evaluateR rel2
  return $ nub (tab1 ++ tab2)
  
evaluateR (INNER_JOIN_ON rel1 rel2 cond) = undefined


---

type SymTab = M.Map String Value
type Evaluator a = State SymTab a

evaluateE :: Typeable a =>  Expression a -> Evaluator Value

evaluateE (LiteralBool x) = return $ BoolValue x
evaluateE (LiteralString x) = return $ StringValue x
evaluateE (LiteralInt x) = return $ IntValue x
evaluateE (LiteralReal x) = return $ RealValue x

evaluateE (Column str) = do
  symTab <- get
  let val =
        do
          s <- (cast str) :: Maybe String
          v <- M.lookup s symTab
          return v
  case val of
    Just v  -> return v
    Nothing -> error $ "Type error / Undefined variable"

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
    _ -> error $ "Types not supported by And"

evaluateE (Or left right) = do
  lft <- evaluateE left
  rgt <- evaluateE right
  case (lft,rgt) of
    (BoolValue l, BoolValue r) -> return $ BoolValue (l || r)
    _ -> error $ "Types not supported by Or"
    
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

