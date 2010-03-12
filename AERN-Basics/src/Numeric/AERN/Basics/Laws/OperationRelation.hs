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
    joinOfOrderedPair, meetOfOrderedPair, joinAboveOperands, meetBelowOperands,
    partialJoinOfOrderedPair, partialJoinAboveOperands,
    roundedJoinOfOrderedPair, roundedMeetOfOrderedPair
)
where

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

joinOfOrderedPair ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
joinOfOrderedPair (<=) (\/) e1 e2 =
    (e1 <= e2) <===> (e1 \/ e2 == e2)

meetOfOrderedPair ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
meetOfOrderedPair (<=) (/\) e1 e2 =
    (e1 <= e2) <===> (e1 /\ e2 == e1)

joinAboveOperands ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
joinAboveOperands (<=) (\/) e1 e2 =
    (e1 <= (e1 \/ e2)) && (e2 <= (e1 \/ e2))

meetBelowOperands ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
meetBelowOperands (<=) (/\) e1 e2 =
    ((e1 /\ e2) <= e1) && ((e1 /\ e2) <= e2)

partialJoinOfOrderedPair ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> Maybe t) ->
    t -> t -> Bool    
partialJoinOfOrderedPair (<=) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinOfOrderedPair (<=) (assumeTotal2 (\/?)) e1 e2 

partialJoinAboveOperands ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> Maybe t) ->
    t -> t -> Bool    
partialJoinAboveOperands (<=) (\/?) e1 e2 =
    (defined (e1 \/? e2)) ===>
    joinAboveOperands (<=) (assumeTotal2 (\/?)) e1 e2

roundedJoinOfOrderedPair ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
roundedJoinOfOrderedPair (<=) (\/.) e1 e2 =
    (e1 <= e2) ===> (e1 \/. e2 <= e2)

roundedMeetOfOrderedPair ::
    (Eq t) =>
    (t -> t -> Bool) ->
    (t -> t -> t) ->
    t -> t -> Bool    
roundedMeetOfOrderedPair (<=) (/\^) e1 e2 =
    (e1 <= e2) ===> (e1 <= (e1 /\^ e2))

