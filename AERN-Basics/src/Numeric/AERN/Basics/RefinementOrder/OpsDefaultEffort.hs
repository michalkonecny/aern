{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort where

import Numeric.AERN.Basics.RefinementOrder

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>? 
infix 4 ⊏?, ⊑?, ⊒?, ⊐?

infixr 2 <\/>?, <⊔>?, >\/<?, >⊔<?

infixr 3 </\>, <⊓>, >/\<, >⊓< 
infixr 2 <\/>, <⊔>, >\/<, >⊔< 

(|==?), (|<==>?), (|</=>?), (|<?), (|>?), (|<=?), (|>=?) ::
    (PartialComparison t) => 
    t -> t -> Maybe Bool

-- | Partial equality
(|==?) a = pEqualEff (pCompareDefaultEffort a) a
-- | Partial `is comparable to`.
(|<==>?) a = pComparableEff (pCompareDefaultEffort a) a
-- | Partial `is not comparable to`.
(|</=>?) a = pIncomparableEff (pCompareDefaultEffort a) a

(|<?) a = pLessEff (pCompareDefaultEffort a) a
(|<=?) a = pLeqEff (pCompareDefaultEffort a) a
(|>=?) a = pGeqEff (pCompareDefaultEffort a) a
(|>?) a = pGreaterEff (pCompareDefaultEffort a) a


(<\/>?) ::
    (OuterRoundedBasis t) => 
    t -> t -> Maybe t
(<\/>?) a = partialJoinOutEff (partialJoinOutDefaultEffort a) a 

(>\/<?) ::
    (InnerRoundedBasis t) => 
    t -> t -> Maybe t
(>\/<?) a = partialJoinInEff (partialJoinInDefaultEffort a) a 

(<\/>), (</\>) :: 
    (OuterRoundedLattice t) => 
    t -> t -> t
    
(<\/>) a = joinOutEff (joinmeetOutDefaultEffort a) a 
(</\>) a = meetOutEff (joinmeetOutDefaultEffort a) a 


(>\/<), (>/\<) :: 
    (InnerRoundedLattice t) => 
    t -> t -> t
    
(>\/<) a = joinInEff (joinmeetInDefaultEffort a) a 
(>/\<) a = meetInEff (joinmeetInDefaultEffort a) a 

-- convenience Unicode operator notation:
(⊏?), (⊑?), (⊒?), (⊐?) :: 
    (PartialComparison t) => 
    t -> t -> Maybe Bool

(⊏?) = (|<?)
(⊑?) = (|<=?)
(⊒?) = (|>=?)
(⊐?) = (|>?)

{-| convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: 
    (OuterRoundedBasis t) => 
    t -> t -> Maybe t
(<⊔>?) = (<\/>?)

{-| convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: 
    (InnerRoundedBasis t) => 
    t -> t -> Maybe t
(>⊔<?) = (>\/<?)

{-| convenience Unicode notation for '<\/>' -}
(<⊔>) :: 
    (OuterRoundedLattice t) => 
    t -> t -> t
(<⊔>) = (<\/>)
{-| convenience Unicode notation for '</\>' -}
(<⊓>) :: 
    (OuterRoundedLattice t) => 
    t -> t -> t
(<⊓>) = (</\>)

{-| convenience Unicode notation for '>\/<' -}
(>⊔<) :: 
    (InnerRoundedLattice t) => 
    t -> t -> t
(>⊔<) = (>\/<)
{-| convenience Unicode notation for '>/\<' -}
(>⊓<) :: 
    (InnerRoundedLattice t) => 
    t -> t -> t
(>⊓<) = (>/\<)
