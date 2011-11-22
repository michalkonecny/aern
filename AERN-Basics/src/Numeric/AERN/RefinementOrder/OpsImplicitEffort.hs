{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.RefinementOrder.OpsImplicitEffort where

import Numeric.AERN.RefinementOrder

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>? 
infix 4 ⊏?, ⊑?, ⊒?, ⊐?

infixr 2 <\/>?, <⊔>?, >\/<?, >⊔<?

infixr 3 </\>, <⊓>, >/\<, >⊓< 
infixr 2 <\/>, <⊔>, >\/<, >⊔< 


(|==?), (|<==>?), (|</=>?), (|<?), (|>?), (|<=?), (|>=?) ::
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool

(|==?) = pEqualEff ?pCompareEffort
(|<==>?) = pComparableEff ?pCompareEffort
(|</=>?) = pIncomparableEff ?pCompareEffort
(|<?) = pLessEff ?pCompareEffort
(|>?) = pGreaterEff ?pCompareEffort
(|<=?) = pLeqEff ?pCompareEffort
(|>=?) = pGeqEff ?pCompareEffort


(<\/>?) ::
    (RoundedBasis t, ?partialJoinEffort :: PartialJoinEffortIndicator t) => 
    t -> t -> Maybe t
(<\/>?) = partialJoinOutEff ?partialJoinEffort 

(>\/<?) ::
    (RoundedBasis t, ?partialJoinEffort :: PartialJoinEffortIndicator t) => 
    t -> t -> Maybe t
    
(>\/<?) = partialJoinInEff ?partialJoinEffort 


(<\/>), (</\>) :: 
    (RoundedLattice t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
    
(<\/>) = joinOutEff ?joinmeetEffort 
(</\>) = meetOutEff ?joinmeetEffort 


(>\/<), (>/\<) :: 
    (RoundedLattice t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
    
(>\/<) = joinInEff ?joinmeetEffort 
(>/\<) = meetInEff ?joinmeetEffort 



-- convenience Unicode operator notation for order:
(⊏?), (⊑?), (⊒?), (⊐?) :: 
    (PartialComparison t, 
     ?pCompareEffort :: PartialCompareEffortIndicator t) => 
    t -> t -> Maybe Bool

(⊏?) = (|<?)
(⊑?) = (|<=?)
(⊒?) = (|>=?)
(⊐?) = (|>?)

{-| convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: 
    (RoundedBasis t, ?partialJoinEffort :: PartialJoinEffortIndicator t) => 
    t -> t -> Maybe t
(<⊔>?) = (<\/>?)

{-| convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: 
    (RoundedBasis t, ?partialJoinEffort :: PartialJoinEffortIndicator t) => 
    t -> t -> Maybe t
(>⊔<?) = (>\/<?)

{-| convenience Unicode notation for '<\/>' -}
(<⊔>) :: 
    (RoundedLattice t, ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
(<⊔>) = (<\/>)
{-| convenience Unicode notation for '</\>' -}
(<⊓>) :: 
    (RoundedLattice t, ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
(<⊓>) = (</\>)

{-| convenience Unicode notation for '>\/<' -}
(>⊔<) :: 
    (RoundedLattice t, ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
(>⊔<) = (>\/<)
{-| convenience Unicode notation for '>/\<' -}
(>⊓<) :: 
    (RoundedLattice t, ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    t -> t -> t
(>⊓<) = (>/\<)
