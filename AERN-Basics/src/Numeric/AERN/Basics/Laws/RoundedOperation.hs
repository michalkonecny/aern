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

module Numeric.AERN.Basics.Laws.RoundedOperation where

import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

partialRoundedIdempotent :: (PRel t) -> (PartOp t) -> (PartOp t) -> t -> Bool
partialRoundedIdempotent (<=?) (*^?) (*.?) e =
    (defined (e *.? e) && defined (e *^? e))
    ===>
    (roundedIdempotent  (<=?) (*^) (*.) e)
    where
    (*.) = assumeTotal2 (*.?)
    (*^) = assumeTotal2 (*^?)

partialRoundedCommutative :: (PRel t) -> (PartOp t) -> (PartOp t) -> t -> t -> Bool
partialRoundedCommutative (<=?) (*^?) (*.?) e1 e2 =
    (and $ map defined [e1 *.? e2, e2 *.? e1, e1 *^? e2, e2 *^? e1])
    ===>
    (roundedCommutative (<=?) (*^) (*.) e1 e2)
    where
    (*.) = assumeTotal2 (*.?)
    (*^) = assumeTotal2 (*^?)

partialRoundedAssociative :: (PRel t) -> (PartOp t) -> (PartOp t) -> t -> t -> t -> Bool
partialRoundedAssociative (<=?) (*^?) (*.?) e1 e2 e3 =
    (and $ map defined [e1 *.? e2, (e1 *. e2) *.? e3, e2 *.? e3, e1 *.? (e2 *. e3), 
                        e1 *^? e2, (e1 *^ e2) *^? e3, e2 *^? e3, e1 *^? (e2 *^ e3)])
    ===>
    (roundedAssociative (<=?) (*^) (*.) e1 e2 e3)
    where
    (*.) = assumeTotal2 (*.?)
    (*^) = assumeTotal2 (*^?)

roundedUnit :: t -> (PRel t) -> (Op t) -> (Op t) -> t -> Bool
roundedUnit unit =
    equalRoundingUpDn11 (\(*) e -> e) (\(*) e -> unit * e)  

roundedIdempotent :: (PRel t) -> (Op t) -> (Op t) -> t -> Bool
roundedIdempotent =
    equalRoundingUpDn11 (\(*) e -> e) (\(*) e -> e * e)  

roundedCommutative :: (PRel t) -> (Op t) -> (Op t) -> t -> t -> Bool
roundedCommutative = 
    equalRoundingUpDn12 (\(*) e1 e2 -> e1 * e2) (\(*) e1 e2 -> e2 * e1)  


roundedAssociative :: (PRel t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedAssociative = 
    equalRoundingUpDn13 
        (\(*) e1 e2 e3 -> (e1 * e2) * e3) 
        (\(*) e1 e2 e3 -> e1 * (e2 * e3))  

roundedModular :: 
    (PRel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedModular = 
    equalRoundingUpDn23 
        (\(/\) (\/) e1 e2 e3 -> (e1 /\ e3) \/ (e2 /\ e3)) 
        (\(/\) (\/) e1 e2 e3 -> ((e1 /\ e3) \/ e2) /\ e3)  

roundedLeftDistributive :: 
    (PRel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
roundedLeftDistributive = 
    equalRoundingUpDn23 
        (\(/\) (\/) e1 e2 e3 -> e1 \/ (e2 /\ e3)) 
        (\(/\) (\/) e1 e2 e3 -> (e1 \/ e2) /\ (e1 \/ e3))     

equalRoundingUpDn11 :: (Expr1Op1 t) -> (Expr1Op1 t) -> (PRel t) -> (Op t) -> (Op t) -> t -> Bool
equalRoundingUpDn11 expr1 expr2  (<=?) (*^) (*.) e =
    (defined (expr1 (*.) e <=? (expr2 (*^) e)) 
        ===> (expr1 (*.) e <= (expr2 (*^) e)))
    && 
    (defined (expr2 (*.) e <=? (expr1 (*^) e)) 
        ===> (expr2 (*.) e <= (expr1 (*^) e)))
    where
    (<=) = assumeTotal2 (<=?)
    
equalRoundingUpDn12 :: (Expr1Op2 t) -> (Expr1Op2 t) -> (PRel t) -> (Op t) -> (Op t) -> t -> t -> Bool
equalRoundingUpDn12 expr1 expr2  (<=?) (*^) (*.) e1 e2 =
    (defined (expr1 (*.) e1 e2 <=? (expr2 (*^) e1 e2)) 
        ===> (expr1 (*.) e1 e2 <= (expr2 (*^) e1 e2))) 
    && 
    (defined (expr2 (*.) e1 e2 <=? (expr1 (*^) e1 e2)) 
        ===> (expr2 (*.) e1 e2 <= (expr1 (*^) e1 e2)))
    where
    (<=) = assumeTotal2 (<=?)
    
equalRoundingUpDn13 :: (Expr1Op3 t) -> (Expr1Op3 t) -> (PRel t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
equalRoundingUpDn13 expr1 expr2  (<=?) (*^) (*.) e1 e2 e3 =
    (defined (expr1 (*.) e1 e2 e3 <=? (expr2 (*^) e1 e2 e3)) 
        ===> (expr1 (*.) e1 e2 e3 <= (expr2 (*^) e1 e2 e3)))
    && 
    (defined (expr2 (*.) e1 e2 e3 <=? (expr1 (*^) e1 e2 e3))
        ===> (expr2 (*.) e1 e2 e3 <= (expr1 (*^) e1 e2 e3)))
    where
    (<=) = assumeTotal2 (<=?)
    
equalRoundingUpDn23 :: (Expr2Op3 t) -> (Expr2Op3 t) -> (PRel t) -> (Op t) -> (Op t) -> (Op t) -> (Op t) -> t -> t -> t -> Bool
equalRoundingUpDn23 expr1 expr2  (<=?) (*^) (**^) (*.) (**.) e1 e2 e3 =
    (defined (expr1 (*.) (**.) e1 e2 e3 <=? (expr2 (*^) (**^) e1 e2 e3)) 
        ===> (expr1 (*.) (**.) e1 e2 e3 <= (expr2 (*^) (**^) e1 e2 e3)))
    && 
    (defined (expr2 (*.) (**.) e1 e2 e3 <=? (expr1 (*^) (**^) e1 e2 e3)) 
        ===> (expr2 (*.) (**.) e1 e2 e3 <= (expr1 (*^) (**^) e1 e2 e3)))
    where
    (<=) = assumeTotal2 (<=?)
    
     