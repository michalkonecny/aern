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

import Numeric.AERN.Basics.Effort

type Expr1 t = t -> t
type Expr2 t = t -> t -> t
type Expr3 t = t -> t -> t -> t

type ExprEff1 ei t = ei -> t -> t
type ExprEff2 ei t = ei -> t -> t -> t
type ExprEff3 ei t = ei -> t -> t -> t -> t

type Op t = t -> t -> t
type PartOp t = t -> t -> Maybe t

type OpEff ei t = ei -> t -> t -> t
type PartOpEff ei t = ei -> t -> t -> Maybe t

type Rel t = t -> t -> Bool
type SmdcRel t = t -> t -> Maybe Bool
type SmdcRelEff ei t = ei -> t -> t -> Maybe Bool

type Expr1Op1 t = (Op t) -> Expr1 t
type Expr1Op2 t = (Op t) -> Expr2 t
type Expr1Op3 t = (Op t) -> Expr3 t
type Expr2Op1 t = (Op t) -> (Op t) -> Expr1 t
type Expr2Op2 t = (Op t) -> (Op t) -> Expr2 t
type Expr2Op3 t = (Op t) -> (Op t) -> Expr3 t

type Expr1Op1Eff ei t = (OpEff ei t) -> ExprEff1 ei t
type Expr1Op2Eff ei t = (OpEff ei t) -> ExprEff2 ei t
type Expr1OpEff3 ei t = (OpEff ei t) -> ExprEff3 ei t
type Expr2OpEff1 ei1 ei2 t = (OpEff ei1 t) -> (OpEff ei2 t) -> ExprEff1 (ei1, ei2) t
type Expr2OpEff2 ei1 ei2 t = (OpEff ei1 t) -> (OpEff ei2 t) -> ExprEff2 (ei1, ei2) t
type Expr2OpEff3 ei1 ei2 t = (OpEff ei1 t) -> (OpEff ei2 t) -> ExprEff3 (ei1, ei2) t
