{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.InPlace.RoundedLattice
    Description :  lattices with directed-rounded in-place operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with directed-rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent RefinementOrder. 
-}
module Numeric.AERN.RefinementOrder.InPlace.RoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.RefinementOrder.Arbitrary
import Numeric.AERN.RefinementOrder.PartialComparison 
import Numeric.AERN.RefinementOrder.Extrema
import Numeric.AERN.RefinementOrder.RoundedLattice

import Numeric.AERN.Basics.Laws.RoundedOpInPlace

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

infixr 3 </\>=, >/\<=, <⊓>=, >⊓<= 
infixr 2 <\/>=, >\/<=, <⊔>=, >⊔<= 

{-|
    A type with directed-rounding lattice operations.
-}
class 
    (RoundedLatticeEffort t, CanBeMutable t) => 
    RoundedLatticeInPlace t 
    where
    joinInInPlaceEff :: OpMutable2Eff (JoinMeetEffortIndicator t) t s
    joinOutInPlaceEff :: OpMutable2Eff (JoinMeetEffortIndicator t) t s
    meetInInPlaceEff :: OpMutable2Eff (JoinMeetEffortIndicator t) t s
    meetOutInPlaceEff :: OpMutable2Eff (JoinMeetEffortIndicator t) t s
    
-- | Outward rounded in-place meet with default effort
meetOutInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
meetOutInPlace = mutable2EffToMutable2 meetOutInPlaceEff joinmeetDefaultEffort 

-- | Outward rounded meet assignment with default effort
(</\>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(</\>=) = mutable2ToMutable1 meetOutInPlace

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<⊓>=) = (</\>=)

-- | Inward rounded in-place meet with default effort
meetInInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
meetInInPlace = mutable2EffToMutable2 meetInInPlaceEff joinmeetDefaultEffort 

-- | Inward rounded meet assignment with default effort
(>/\<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>/\<=) = mutable2ToMutable1 meetInInPlace

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>⊓<=) = (>/\<=)

-- | Outward rounded in-place join with default effort
joinOutInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
joinOutInPlace = mutable2EffToMutable2 joinOutInPlaceEff joinmeetDefaultEffort 

-- | Outward rounded join assignment with default effort
(<\/>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOutInPlace

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<⊔>=) = (<\/>=)

-- | Inward rounded in-place join with default effort
joinInInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
joinInInPlace = mutable2EffToMutable2 joinInInPlaceEff joinmeetDefaultEffort 

-- | Inward rounded join assignment with default effort
(>\/<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinInInPlace

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>⊔<=) = (>\/<=)
    
    
joinOutInPlaceEffFromPure, 
 meetOutInPlaceEffFromPure :: 
    (CanBeMutable t, RoundedLattice t) => 
    OpMutable2Eff (JoinMeetEffortIndicator t) t s  
joinOutInPlaceEffFromPure = pureToMutable2Eff joinOutEff 
meetOutInPlaceEffFromPure = pureToMutable2Eff meetOutEff 

joinOutEffFromInPlace,
 meetOutEffFromInPlace ::
 (CanBeMutable t, RoundedLatticeInPlace t) =>
 (JoinMeetEffortIndicator t) -> t -> t -> t
joinOutEffFromInPlace = mutable2EffToPure joinOutInPlaceEff 
meetOutEffFromInPlace = mutable2EffToPure meetOutInPlaceEff 

joinInInPlaceEffFromPure, 
 meetInInPlaceEffFromPure :: 
    (CanBeMutable t, RoundedLattice t) => 
    OpMutable2Eff (JoinMeetEffortIndicator t) t s  
joinInInPlaceEffFromPure = pureToMutable2Eff joinInEff 
meetInInPlaceEffFromPure = pureToMutable2Eff meetInEff 

joinInEffFromInPlace,
 meetInEffFromInPlace ::
 (CanBeMutable t, RoundedLatticeInPlace t) =>
 (JoinMeetEffortIndicator t) -> t -> t -> t
joinInEffFromInPlace = mutable2EffToPure joinInInPlaceEff 
meetInEffFromInPlace = mutable2EffToPure meetInInPlaceEff 

propInOutRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RoundedLatticeInPlace t, 
     RoundedLattice t, 
     CanBeMutable t) =>
    t -> 
    (JoinMeetEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propInOutRoundedLatticeJoinInPlaceConsistentWithPure 
    _ (joinmeetEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (joinOutInPlaceEff joinmeetEffort)  
        (joinInInPlaceEff joinmeetEffort)
        (joinOutEff joinmeetEffort) 
        (joinInEff joinmeetEffort) 
        e1 e2  

propInOutRoundedLatticeMeetInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RoundedLatticeInPlace t,
     RoundedLattice t, 
     CanBeMutable t) => 
    t -> 
    (JoinMeetEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propInOutRoundedLatticeMeetInPlaceConsistentWithPure 
    _ (joinmeetEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (meetOutInPlaceEff joinmeetEffort)  
        (meetInInPlaceEff joinmeetEffort)
        (meetOutEff joinmeetEffort) 
        (meetInEff joinmeetEffort) 
        e1 e2  

testsOuterInnerRoundedLatticeInPlace :: 
    (PartialComparison t,
     RoundedLatticeInPlace t, 
     RoundedLattice t, 
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (JoinMeetEffortIndicator t), Show (JoinMeetEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsOuterInnerRoundedLatticeInPlace (name, sample) =
    testGroup (name ++ " (join,meet) rounded in-place") $
        [
         testProperty "join in-place=pure"
             (propInOutRoundedLatticeJoinInPlaceConsistentWithPure sample)
        ,
         testProperty "meet in-place=pure"
             (propInOutRoundedLatticeMeetInPlaceConsistentWithPure sample)
        ]

