module TableOps where

import Control.Monad
import Data.List (nub)
import Table

selectOp :: [String] -> Table -> Table
selectOp colNames = map filterRow where
  filterRow row = row >>= (\cell -> if elem (fst cell) colNames then [cell] else [])

whereOp :: (Row -> Bool) -> Table -> Table
whereOp f tab = tab >>= (\row -> if f row then [row] else [])

unionOp :: Table -> Table -> Table
unionOp tab1 tab2 = nub (tab1 ++ tab2)

innerJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
innerJoin f ls rs = filter (uncurry f) (liftM2 (,) ls rs)

  
