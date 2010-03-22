{-|
    Module      :  Numeric.AERN.Basics.Laws.Utilities
    Description :  utilities for easier definitions of algebraic laws 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Utilities for easier definitions of algebraic laws.
-}
module Numeric.AERN.Basics.Laws.Utilities where

type Expr1 t = t -> t
type Expr2 t = t -> t -> t
type Expr3 t = t -> t -> t -> t

type Op t = t -> t -> t
type PartOp t = t -> t -> Maybe t

type Rel t = t -> t -> Bool
type SmdcRel t = t -> t -> Maybe Bool

type Expr1Op1 t = (Op t) -> Expr1 t
type Expr1Op2 t = (Op t) -> Expr2 t
type Expr1Op3 t = (Op t) -> Expr3 t
type Expr2Op1 t = (Op t) -> (Op t) -> Expr1 t
type Expr2Op2 t = (Op t) -> (Op t) -> Expr2 t
type Expr2Op3 t = (Op t) -> (Op t) -> Expr3 t
