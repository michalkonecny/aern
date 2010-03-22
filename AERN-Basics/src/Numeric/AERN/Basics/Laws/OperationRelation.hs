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
    joinOfOrderedPairNonReflexive, meetOfOrderedPairNonReflexive, 
    joinOfOrderedPair, meetOfOrderedPair, 
    joinAboveOperands, meetBelowOperands,
    partialJoinOfOrderedPair, partialJoinAboveOperands,
    roundedJoinOfOrderedPair, roundedMeetOfOrderedPair
)
where

import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

joinOfOrderedPairNonReflexive ::
    (Rel t) -> (Rel t) -> (Op t) -> t -> t -> Bool    
joinOfOrderedPairNonReflexive (==) (<=) (\/) e1 e2 =
    (e1 <= e2) ===> ((e1 \/ e2) == e2)

meetOfOrderedPairNonReflexive ::
    (Rel t) -> (Rel t) -> (Op t) -> t -> t -> Bool    
meetOfOrderedPairNonReflexive (==) (<=) (/\) e1 e2 =
    (e1 <= e2) ===> ((e1 /\ e2) == e1)

joinOfOrderedPair ::
    (Rel t) -> (Rel t) -> (Op t) -> t -> t -> Bool    
joinOfOrderedPair (==) (<=) (\/) e1 e2 =
    (e1 <= e2) <===> ((e1 \/ e2) == e2)

meetOfOrderedPair ::
    (Rel t) -> (Rel t) -> (Op t) -> t -> t -> Bool    
meetOfOrderedPair (==) (<=) (/\) e1 e2 =
    (e1 <= e2) <===> ((e1 /\ e2) == e1)

joinAboveOperands ::
    (Rel t) -> (Op t) -> t -> t -> Bool    
joinAboveOperands (<=) (\/) e1 e2 =
    (e1 <= (e1 \/ e2)) && (e2 <= (e1 \/ e2))

meetBelowOperands ::
    (Rel t) -> (Op t) -> t -> t -> Bool    
meetBelowOperands (<=) (/\) e1 e2 =
    ((e1 /\ e2) <= e1) && ((e1 /\ e2) <= e2)

partialJoinOfOrderedPair ::
    (Rel t) -> (Rel t) -> (PartOp t) -> t -> t -> Bool    
partialJoinOfOrderedPair (==) (<=) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinOfOrderedPair (==) (<=) (assumeTotal2 (\/?)) e1 e2 

partialJoinAboveOperands ::
    (Rel t) -> (PartOp t) -> t -> t -> Bool    
partialJoinAboveOperands (<=) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinAboveOperands (<=) (assumeTotal2 (\/?)) e1 e2

roundedJoinOfOrderedPair ::
    (Rel t) -> (Op t) -> t -> t -> Bool    
roundedJoinOfOrderedPair (<=) (\/.) e1 e2 =
    (e1 <= e2) ===> (e1 \/. e2 <= e2)

roundedMeetOfOrderedPair ::
    (Rel t) -> (Op t) -> t -> t -> Bool    
roundedMeetOfOrderedPair (<=) (/\^) e1 e2 =
    (e1 <= e2) ===> (e1 <= (e1 /\^ e2))

