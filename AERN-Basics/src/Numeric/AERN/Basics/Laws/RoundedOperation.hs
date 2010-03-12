{-|
    Module      :  Numeric.AERN.Basics.Laws.Relation
    Description :  common properties of rounded binary operations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common properties of rounded binary operations.
-}

module Numeric.AERN.Basics.Laws.RoundedOperation 

where

type Expr1 t = t -> t
type Expr2 t = t -> t -> t
type Expr3 t = t -> t -> t -> t

type Op t = t -> t -> t
type Rel t = t -> t -> Bool

type Expr11 t = (Op t) -> Expr1 t
type Expr12 t = (Op t) -> Expr2 t
type Expr13 t = (Op t) -> Expr3 t
type Expr21 t = (Op t) -> (Op t) -> Expr1 t
type Expr22 t = (Op t) -> (Op t) -> Expr2 t
type Expr23 t = (Op t) -> (Op t) -> Expr3 t

roundedIdempotent :: (Rel t) -> (Op t) -> (Op t) -> t -> Bool
roundedIdempotent =
    equalRoundingUpDn11 (\(*) e -> e) (\(*) e -> e * e)  

roundedCommutative :: (Rel t) -> (Op t) -> (Op t) -> t -> t -> Bool
roundedCommutative = 
    equalRoundingUpDn12 (\(*) e1 e2 -> e1 * e2) (\(*) e1 e2 -> e2 * e1)  


roundedAssociative :: (Eq t) => (Rel t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedAssociative = 
    equalRoundingUpDn13 
        (\(*) e1 e2 e3 -> (e1 * e2) * e3) 
        (\(*) e1 e2 e3 -> e1 * (e2 * e3))  

roundedModular :: 
    (Eq t) => 
    (Rel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedModular = 
    equalRoundingUpDn23 
        (\(/\) (\/) e1 e2 e3 -> (e1 /\ e3) \/ (e2 /\ e3)) 
        (\(/\) (\/) e1 e2 e3 -> ((e1 /\ e3) \/ e2) /\ e3)  

roundedLeftDistributive :: 
    (Eq t) => 
    (Rel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedLeftDistributive = 
    equalRoundingUpDn23 
        (\(/\) (\/) e1 e2 e3 -> e1 \/ (e2 /\ e3)) 
        (\(/\) (\/) e1 e2 e3 -> (e1 \/ e2) /\ (e1 \/ e3))     

equalRoundingUpDn11 :: (Expr11 t) -> (Expr11 t) -> (Rel t) -> (Op t) -> (Op t) -> t -> Bool
equalRoundingUpDn11 expr1 expr2  (<=) (*^) (*.) e =
    (expr1 (*.) e <= (expr2 (*^) e))
    &&
    (expr2 (*.) e <= (expr1 (*^) e))
    
equalRoundingUpDn12 :: (Expr12 t) -> (Expr12 t) -> (Rel t) -> (Op t) -> (Op t) -> t -> t -> Bool
equalRoundingUpDn12 expr1 expr2  (<=) (*^) (*.) e1 e2 =
    (expr1 (*.) e1 e2 <= (expr2 (*^) e1 e2))
    &&
    (expr2 (*.) e1 e2 <= (expr1 (*^) e1 e2))
    
equalRoundingUpDn13 :: (Expr13 t) -> (Expr13 t) -> (Rel t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
equalRoundingUpDn13 expr1 expr2  (<=) (*^) (*.) e1 e2 e3 =
    (expr1 (*.) e1 e2 e3 <= (expr2 (*^) e1 e2 e3))
    &&
    (expr2 (*.) e1 e2 e3 <= (expr1 (*^) e1 e2 e3))
    
equalRoundingUpDn23 :: (Expr23 t) -> (Expr23 t) -> (Rel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
equalRoundingUpDn23 expr1 expr2  (<=) (*^) (**^) (*.) (**.) e1 e2 e3 =
    (expr1 (*.) (**.) e1 e2 e3 <= (expr2 (*^) (**^) e1 e2 e3))
    &&
    (expr2 (*.) (**.) e1 e2 e3 <= (expr1 (*^) (**^) e1 e2 e3))
    
     