{-|
    Module      :  Numeric.AERN.Basics.Laws.OperationRelation
    Description :  common properties linking binary operations with relations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common properties linking binary operations with relations.
-}
module Numeric.AERN.Basics.Laws.OperationRelation 
(
    joinMeetOfOrderedPair, joinAboveOperands, meetBelowOperands,
    partialJoinOfOrderedPair, partialJoinAboveOperands
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

partialJoinOfOrderedPair (<=) (\/?) e1 e2 =
    ((not (e1 <= e2)) || (e1 \/? e2 == Just e2))
    &&
    ((e1 <= e2) || (not (e1 \/? e2 == Just e2)))

partialJoinAboveOperands (<=) (\/?) e1 e2 =
    case e1 \/? e2 of 
        Nothing -> True
        Just e ->  (e1 <= e) && (e2 <= e)
