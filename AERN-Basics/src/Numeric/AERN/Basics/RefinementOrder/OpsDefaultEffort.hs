{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
    Description :  Convenience binary infix operators with default effort parameters  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort where

import Numeric.AERN.Basics.RefinementOrder

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>?, ⊏?, ⊑?, ⊒?, ⊐?
infixr 3 </\>, >/\<, <⊓>, >⊓< 
infixr 2 <\/>?, >\/<?, <\/>, >\/<, <⊔>?, >⊔<?, <⊔>, >⊔< 

-- | Partial equality
(|==?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|==?) a = pEqualEff (pCompareDefaultEffort a) a

-- | Partial `is comparable to`
(|<==>?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|<==>?) a = pComparableEff (pCompareDefaultEffort a) a

-- | Partial `is not comparable to`
(|</=>?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|</=>?) a = pIncomparableEff (pCompareDefaultEffort a) a

-- | Partial `strictly below`
(|<?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|<?) a = pLessEff (pCompareDefaultEffort a) a

-- | Partial `below or equal to`
(|<=?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|<=?) a = pLeqEff (pCompareDefaultEffort a) a

-- | Partial `above or equal to`
(|>=?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|>=?) a = pGeqEff (pCompareDefaultEffort a) a

-- | Partial `strictly above`
(|>?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|>?) a = pGreaterEff (pCompareDefaultEffort a) a

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊏?) = (|<?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊑?) = (|<=?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊒?) = (|>=?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊐?) = (|>?)

-- | Outward rounded meet
(</\>) :: (OuterRoundedLattice t) => t -> t -> t
(</\>) a = meetOutEff (joinmeetOutDefaultEffort a) a 

-- | Inward rounded meet
(>/\<) :: (InnerRoundedLattice t) => t -> t -> t
(>/\<) a = meetInEff (joinmeetInDefaultEffort a) a 

-- | Partial outward rounded join
(<\/>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t
(<\/>?) a = partialJoinOutEff (partialJoinOutDefaultEffort a) a 

-- | Partial inward rounded join
(>\/<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>\/<?) a = partialJoinInEff (partialJoinInDefaultEffort a) a 

-- | Outward rounded join
(<\/>) :: (OuterRoundedLattice t) => t -> t -> t 
(<\/>) a = joinOutEff (joinmeetOutDefaultEffort a) a 

-- | Inward rounded join
(>\/<) :: (InnerRoundedLattice t) => t -> t -> t
(>\/<) a = joinInEff (joinmeetInDefaultEffort a) a 

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t 
(<⊔>?) = (<\/>?)

{-| Convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>⊔<?) = (>\/<?)

{-| Convenience Unicode notation for '<\/>' -}
(<⊔>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊔>) = (<\/>)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊓>) = (</\>)

{-| Convenience Unicode notation for '>\/<' -}
(>⊔<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊔<) = (>\/<)

{-| Convenience Unicode notation for '>/\<' -}
(>⊓<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊓<) = (>/\<)
