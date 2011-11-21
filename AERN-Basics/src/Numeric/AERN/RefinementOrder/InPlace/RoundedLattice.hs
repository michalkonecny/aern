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

{-|
    A type with directed-rounding lattice operations.
-}
class 
    (OuterRoundedLatticeEffort t, CanBeMutable t) => 
    OuterRoundedLatticeInPlace t 
    where
    joinOutInPlaceEff :: OpMutable2Eff (JoinMeetOutEffortIndicator t) t s
    meetOutInPlaceEff :: OpMutable2Eff (JoinMeetOutEffortIndicator t) t s
    
joinOutInPlaceEffFromPure, 
 meetOutInPlaceEffFromPure :: 
    (CanBeMutable t, OuterRoundedLattice t) => 
    OpMutable2Eff (JoinMeetOutEffortIndicator t) t s  
joinOutInPlaceEffFromPure = pureToMutable2Eff joinOutEff 
meetOutInPlaceEffFromPure = pureToMutable2Eff meetOutEff 

joinOutEffFromInPlace,
 meetOutEffFromInPlace ::
 (CanBeMutable t, OuterRoundedLatticeInPlace t) =>
 (JoinMeetOutEffortIndicator t) -> t -> t -> t
joinOutEffFromInPlace = mutable2EffToPure joinOutInPlaceEff 
meetOutEffFromInPlace = mutable2EffToPure meetOutInPlaceEff 

class 
    (InnerRoundedLatticeEffort t, CanBeMutable t) => 
    InnerRoundedLatticeInPlace t 
    where
    joinInInPlaceEff :: OpMutable2Eff (JoinMeetInEffortIndicator t) t s
    meetInInPlaceEff :: OpMutable2Eff (JoinMeetInEffortIndicator t) t s
    
joinInInPlaceEffFromPure, 
 meetInInPlaceEffFromPure :: 
    (CanBeMutable t, InnerRoundedLattice t) => 
    OpMutable2Eff (JoinMeetInEffortIndicator t) t s  
joinInInPlaceEffFromPure = pureToMutable2Eff joinInEff 
meetInInPlaceEffFromPure = pureToMutable2Eff meetInEff 

joinInEffFromInPlace,
 meetInEffFromInPlace ::
 (CanBeMutable t, InnerRoundedLatticeInPlace t) =>
 (JoinMeetInEffortIndicator t) -> t -> t -> t
joinInEffFromInPlace = mutable2EffToPure joinInInPlaceEff 
meetInEffFromInPlace = mutable2EffToPure meetInInPlaceEff 

propOuterInnerRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     OuterRoundedLatticeInPlace t, InnerRoundedLatticeInPlace t, 
     RoundedLattice t, 
     CanBeMutable t) =>
    t -> 
    (JoinMeetOutEffortIndicator t, JoinMeetInEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propOuterInnerRoundedLatticeJoinInPlaceConsistentWithPure 
    _ (joinmeetOutEffort, joinmeetInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (joinOutInPlaceEff joinmeetOutEffort)  
        (joinInInPlaceEff joinmeetInEffort)
        (joinOutEff joinmeetOutEffort) 
        (joinInEff joinmeetInEffort) 
        e1 e2  

propOuterInnerRoundedLatticeMeetInPlaceConsistentWithPure ::
    (PartialComparison t, 
     OuterRoundedLatticeInPlace t, InnerRoundedLatticeInPlace t,
     RoundedLattice t, 
     CanBeMutable t) => 
    t -> 
    (JoinMeetOutEffortIndicator t, JoinMeetInEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propOuterInnerRoundedLatticeMeetInPlaceConsistentWithPure 
    _ (joinmeetOutEffort, joinmeetInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (meetOutInPlaceEff joinmeetOutEffort)  
        (meetInInPlaceEff joinmeetInEffort)
        (meetOutEff joinmeetOutEffort) 
        (meetInEff joinmeetInEffort) 
        e1 e2  

testsOuterInnerRoundedLatticeInPlace :: 
    (PartialComparison t,
     OuterRoundedLatticeInPlace t, InnerRoundedLatticeInPlace t, 
     RoundedLattice t, 
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (JoinMeetOutEffortIndicator t), Show (JoinMeetOutEffortIndicator t), 
     Arbitrary (JoinMeetInEffortIndicator t), Show (JoinMeetInEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsOuterInnerRoundedLatticeInPlace (name, sample) =
    testGroup (name ++ " (join,meet) rounded in-place") $
        [
         testProperty "join in-place=pure"
             (propOuterInnerRoundedLatticeJoinInPlaceConsistentWithPure sample)
        ,
         testProperty "meet in-place=pure"
             (propOuterInnerRoundedLatticeMeetInPlaceConsistentWithPure sample)
        ]

