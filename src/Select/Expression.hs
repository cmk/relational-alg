{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}

module Select.Expression
  ( Expression (..)
  , Named (..)
  , As
  ) where

-- AST for SQL expressions
data Expression variable
  = LiteralBool Bool
  | LiteralInt Int
  | LiteralReal Double
  | LiteralString String
  | Column variable
  | Not (Expression variable)
  | And (Expression variable) (Expression variable)
  | Or  (Expression variable) (Expression variable)
  | Equ (Expression variable) (Expression variable)
  deriving
    ( Read
    , Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    )
