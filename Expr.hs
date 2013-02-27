{-# LANGUAGE GADTs #-}

module Expr where

type Name = String

data Expr where
  Num    :: Int -> Expr
  Index  :: Name -> [Expr] -> Expr
  (:+:)  :: Expr -> Expr -> Expr
  (:-:)  :: Expr -> Expr -> Expr
  (:*:)  :: Expr -> Expr -> Expr
  (:/:)  :: Expr -> Expr -> Expr
  (:<=:) :: Expr -> Expr -> Expr

instance Eq Expr where

type Size  = Expr
type Index = Expr 

var :: Name -> Expr
var v = Index v []
