{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}

module Select.Relation
  ( Relation (..)
  , RelationIdentifier
  ) where

import Select.Expression

infix 1 `FROM`
infixr 2 `UNION`
infix 3 `INNER_JOIN_ON`
infix 4 `WHERE`

-- Relation abstract syntax tree
data Relation scope variable table
  = TABLE table
  | FROM
  | WHERE
  deriving (Read,Show,Eq,Functor,Foldable,Traversable)

-- Identifier for our toy SQL relation
type RelationIdentifier = Relation String String FilePath
