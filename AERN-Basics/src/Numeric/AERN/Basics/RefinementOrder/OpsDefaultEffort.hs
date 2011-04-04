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

-- | Partial equality
infix 4 |==?
(|==?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|==?) a = pEqualEff (pCompareDefaultEffort a) a

-- | Partial `is comparable to`.
infix 4 |<==>?
(|<==>?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|<==>?) a = pComparableEff (pCompareDefaultEffort a) a

-- | Partial `is not comparable to`.
infix 4 |</=>?
(|</=>?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|</=>?) a = pIncomparableEff (pCompareDefaultEffort a) a

-- | Partial `strictly below`.
infix 4 |<?
(|<?) :: (PartialComparison t) => t -> t -> Maybe Bool
(|<?) a = pLessEff (pCompareDefaultEffort a) a

-- | Partial `below or equal to`.
infix 4 |<=?
(|<=?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|<=?) a = pLeqEff (pCompareDefaultEffort a) a

-- | Partial `above or equal to`.
infix 4 |>=?
(|>=?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|>=?) a = pGeqEff (pCompareDefaultEffort a) a

-- | Partial `strictly above`.
infix 4 |>?
(|>?) :: (PartialComparison t) => t -> t -> Maybe Bool 
(|>?) a = pGreaterEff (pCompareDefaultEffort a) a

{-| convenience Unicode notation for '|<?' -}
infix 4 ⊏?
(⊏?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊏?) = (|<?)

{-| convenience Unicode notation for '|<=?' -}
infix 4 ⊑?
(⊑?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊑?) = (|<=?)

{-| convenience Unicode notation for '|>=?' -}
infix 4 ⊒?
(⊒?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊒?) = (|>=?)

{-| convenience Unicode notation for '|>?' -}
infix 4 ⊐?
(⊐?) :: (PartialComparison t) => t -> t -> Maybe Bool
(⊐?) = (|>?)

-- | Outward rounded meet.
infixr 3 </\>
(</\>) :: (OuterRoundedLattice t) => t -> t -> t
(</\>) a = meetOutEff (joinmeetOutDefaultEffort a) a 

-- | Inward rounded meet.
infixr 3 >/\<
(>/\<) :: (InnerRoundedLattice t) => t -> t -> t
(>/\<) a = meetInEff (joinmeetInDefaultEffort a) a 

-- | Partial outward rounded join.
infixr 2 <\/>?
(<\/>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t
(<\/>?) a = partialJoinOutEff (partialJoinOutDefaultEffort a) a 

-- | Partial inward rounded join.
infixr 2 >\/<?
(>\/<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>\/<?) a = partialJoinInEff (partialJoinInDefaultEffort a) a 

-- | Outward rounded join.
infixr 2 <\/>
(<\/>) :: (OuterRoundedLattice t) => t -> t -> t 
(<\/>) a = joinOutEff (joinmeetOutDefaultEffort a) a 

-- | Inward rounded join.
infixr 2 >\/<
(>\/<) :: (InnerRoundedLattice t) => t -> t -> t
(>\/<) a = joinInEff (joinmeetInDefaultEffort a) a 

{-| convenience Unicode notation for '<\/>?' -}
infixr 2 <⊔>?
(<⊔>?) :: 
    (OuterRoundedBasis t) => 
    t -> t -> Maybe t
(<⊔>?) = (<\/>?)

infixr 2 >⊔<?
{-| convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: 
    (InnerRoundedBasis t) => 
    t -> t -> Maybe t
(>⊔<?) = (>\/<?)

infixr 2 <⊔>
{-| convenience Unicode notation for '<\/>' -}
(<⊔>) :: 
    (OuterRoundedLattice t) => 
    t -> t -> t
(<⊔>) = (<\/>)

infixr 3 <⊓>
{-| convenience Unicode notation for '</\>' -}
(<⊓>) :: 
    (OuterRoundedLattice t) => 
    t -> t -> t
(<⊓>) = (</\>)

infixr 2 >⊔< 
{-| convenience Unicode notation for '>\/<' -}
(>⊔<) :: 
    (InnerRoundedLattice t) => 
    t -> t -> t
(>⊔<) = (>\/<)

infixr 3 >⊓< 
{-| convenience Unicode notation for '>/\<' -}
(>⊓<) :: 
    (InnerRoundedLattice t) => 
    t -> t -> t
(>⊓<) = (>/\<)
