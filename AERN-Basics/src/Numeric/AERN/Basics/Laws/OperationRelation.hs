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
--    joinOfOrderedPairNonReflexive, meetOfOrderedPairNonReflexive, 
    joinOfOrderedPair, meetOfOrderedPair, 
    joinAboveOperands, meetBelowOperands,
    partialJoinOfOrderedPair, partialJoinAboveOperands,
    downRoundedJoinOfOrderedPair, upRoundedMeetOfOrderedPair,
    downRoundedPartialJoinOfOrderedPair
)
where

import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

--joinOfOrderedPairNonReflexive ::
--    (SmdcRel t) -> (SmdcRel t) -> (Op t) -> t -> t -> Bool    
--joinOfOrderedPairNonReflexive (==) (<=) (\/) e1 e2 =
--    (e1 <= e2) ===> ((e1 \/ e2) == e2)
--
--meetOfOrderedPairNonReflexive ::
--    (SmdcRel t) -> (SmdcRel t) -> (Op t) -> t -> t -> Bool    
--meetOfOrderedPairNonReflexive (==) (<=) (/\) e1 e2 =
--    (e1 <= e2) ===> ((e1 /\ e2) == e1)

joinOfOrderedPair ::
    (Rel t) -> (SmdcRel t) -> (Op t) -> t -> t -> Bool    
joinOfOrderedPair (==) (<=?) (\/) e1 e2 =
    (defined (e1 <=? e2))
    ===>
    ((e1 <= e2) <===> ((e1 \/ e2) == e2))
    where
    (<=) = assumeTotal2 (<=?)

meetOfOrderedPair ::
    (Rel t) -> (SmdcRel t) -> (Op t) -> t -> t -> Bool    
meetOfOrderedPair (==) (<=?) (/\) e1 e2 =
    (defined (e1 <=? e2)) 
    ===>
    ((e1 <= e2) <===> ((e1 /\ e2) == e1))
    where
    (<=) = assumeTotal2 (<=?)

joinAboveOperands ::
    (SmdcRel t) -> (Op t) -> t -> t -> Bool
joinAboveOperands (<=?) (\/) e1 e2 =
    (defined (e1 <=? (e1 \/ e2)) && defined (e2 <=? (e1 \/ e2))) 
    ===>
    ((e1 <= (e1 \/ e2)) && (e2 <= (e1 \/ e2)))
    where
    (<=) = assumeTotal2 (<=?)

meetBelowOperands ::
    (SmdcRel t) -> (Op t) -> t -> t -> Bool    
meetBelowOperands (<=?) (/\) e1 e2 =
    (defined ((e1 /\ e2) <=? e1) && defined ((e1 /\ e2) <=? e2)) 
    ===>
    (((e1 /\ e2) <= e1) && ((e1 /\ e2) <= e2))
    where
    (<=) = assumeTotal2 (<=?)

partialJoinOfOrderedPair ::
    (Rel t) -> (SmdcRel t) -> (PartOp t) -> t -> t -> Bool    
partialJoinOfOrderedPair (==) (<=?) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinOfOrderedPair (==) (<=?) (assumeTotal2 (\/?)) e1 e2 

partialJoinAboveOperands ::
    (SmdcRel t) -> (PartOp t) -> t -> t -> Bool    
partialJoinAboveOperands (<=?) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinAboveOperands (<=?) (assumeTotal2 (\/?)) e1 e2



downRoundedJoinOfOrderedPair ::
    (SmdcRel t) -> (Op t) -> t -> t -> Bool    
downRoundedJoinOfOrderedPair (<=?) (\/.) e1 e2 =
    (defined (e1 <=? e2) && defined (e1 \/. e2 <=? e2)) 
    ===>
    ((e1 <= e2) ===> (e1 \/. e2 <= e2))
    where
    (<=) = assumeTotal2 (<=?)

upRoundedMeetOfOrderedPair ::
    (SmdcRel t) -> (Op t) -> t -> t -> Bool    
upRoundedMeetOfOrderedPair (<=?) (/\^) e1 e2 =
    (defined (e1 <=? e2) && defined (e1 <=? (e1 /\^ e2))) 
    ===>
    ((e1 <= e2) ===> (e1 <= (e1 /\^ e2)))
    where
    (<=) = assumeTotal2 (<=?)

downRoundedPartialJoinOfOrderedPair ::
    (SmdcRel t) -> (PartOp t) -> t -> t -> Bool    
downRoundedPartialJoinOfOrderedPair (<=?) (\/.?) e1 e2 =
    (defined (e1 \/.? e2)) ===>
    downRoundedJoinOfOrderedPair (<=?) (assumeTotal2 (\/.?)) e1 e2 
