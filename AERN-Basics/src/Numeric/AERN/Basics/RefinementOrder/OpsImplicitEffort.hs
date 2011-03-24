{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with implicit effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with implicit effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort where

import Numeric.AERN.Basics.RefinementOrder

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
    (OuterRoundedBasis t, ?partialJoinOutEffort :: PartialJoinOutEffortIndicator t) => 
    t -> t -> Maybe t
(<\/>?) = partialJoinOutEff ?partialJoinOutEffort 

(>\/<?) ::
    (InnerRoundedBasis t, ?partialJoinInEffort :: PartialJoinInEffortIndicator t) => 
    t -> t -> Maybe t
    
(>\/<?) = partialJoinInEff ?partialJoinInEffort 


(<\/>), (</\>) :: 
    (OuterRoundedLattice t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    t -> t -> t
    
(<\/>) = joinOutEff ?joinmeetOutEffort 
(</\>) = meetOutEff ?joinmeetOutEffort 


(>\/<), (>/\<) :: 
    (InnerRoundedLattice t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    t -> t -> t
    
(>\/<) = joinInEff ?joinmeetInEffort 
(>/\<) = meetInEff ?joinmeetInEffort 



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
    (OuterRoundedBasis t, ?partialJoinOutEffort :: PartialJoinOutEffortIndicator t) => 
    t -> t -> Maybe t
(<⊔>?) = (<\/>?)

{-| convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: 
    (InnerRoundedBasis t, ?partialJoinInEffort :: PartialJoinInEffortIndicator t) => 
    t -> t -> Maybe t
(>⊔<?) = (>\/<?)

{-| convenience Unicode notation for '<\/>' -}
(<⊔>) :: 
    (OuterRoundedLattice t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    t -> t -> t
(<⊔>) = (<\/>)
{-| convenience Unicode notation for '</\>' -}
(<⊓>) :: 
    (OuterRoundedLattice t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    t -> t -> t
(<⊓>) = (</\>)

{-| convenience Unicode notation for '>\/<' -}
(>⊔<) :: 
    (InnerRoundedLattice t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    t -> t -> t
(>⊔<) = (>\/<)
{-| convenience Unicode notation for '>/\<' -}
(>⊓<) :: 
    (InnerRoundedLattice t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    t -> t -> t
(>⊓<) = (>/\<)
