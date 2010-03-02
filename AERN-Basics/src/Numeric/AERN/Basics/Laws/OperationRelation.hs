{-|
    Module      :  Numeric.AERN.Basics.Laws.OperationRelation
    Description :  common properties linking binary operations with relations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Laws.OperationRelation 
(
    joinMeetOfOrderedPair, joinAboveOperands, meetBelowOperands
)
where

joinMeetOfOrderedPair (<=) (/\) (\/) e1 e2 =
    ((not (e1 <= e2)) || ((e1 \/ e2 == e2) && (e1 /\ e2 == e1)))
    &&
    (((e1 <= e2) && (e1 \/ e2 == e2)) || (not (e1 /\ e2 == e1)))
    &&
    (((e1 <= e2) && (e1 /\ e2 == e1)) || (not (e1 \/ e2 == e2)))

joinAboveOperands (<=) (\/) e1 e2 =
    (e1 <= (e1 \/ e2)) && (e2 <= (e1 \/ e2))

meetBelowOperands (<=) (/\) e1 e2 =
    ((e1 /\ e2) <= e1) && ((e1 /\ e2) <= e2)

