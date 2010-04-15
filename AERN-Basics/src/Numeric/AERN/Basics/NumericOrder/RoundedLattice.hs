{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.RoundedLattice
    Description :  lattices with directed-rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with directed-rounded operations.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.RoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Comparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary 
import Numeric.AERN.Basics.NumericOrder.PartialComparison 
import Numeric.AERN.Basics.NumericOrder.Extrema

import Numeric.AERN.Basics.Laws.PartialRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with directed-rounding lattice operations.
-}
class RoundedLattice t where
    type MinmaxEffortIndicator t
    minmaxDefaultEffort :: t -> MinmaxEffortIndicator t
    maxUpEff :: MinmaxEffortIndicator t -> t -> t -> t
    maxDnEff :: MinmaxEffortIndicator t -> t -> t -> t
    minUpEff :: MinmaxEffortIndicator t -> t -> t -> t
    minDnEff :: MinmaxEffortIndicator t -> t -> t -> t

    maxUp :: t -> t -> t
    maxDn :: t -> t -> t
    minUp :: t -> t -> t
    minDn :: t -> t -> t
    
    maxUp a b = maxUpEff (minmaxDefaultEffort a) a b 
    maxDn a b = maxDnEff (minmaxDefaultEffort a) a b 
    minUp a b = minUpEff (minmaxDefaultEffort a) a b 
    minDn a b = minDnEff (minmaxDefaultEffort a) a b 


propRoundedLatticeIllegalArgException :: 
    (RoundedLattice t) => 
    t -> t -> Bool
propRoundedLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] | op <- [maxUp, maxDn, minUp, minDn]] 

propRoundedLatticeComparisonCompatible :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible _ (UniformlyOrderedPair (e1,e2)) = 
    (downRoundedJoinOfOrderedPair (<=?) minDn e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (<=?) maxUp e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (<=?) maxUp e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (<=?) minDn e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> t -> Bool
propRoundedLatticeJoinIdempotent _ = roundedIdempotent (<=?) maxUp maxDn

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (<=?) maxUp maxDn e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (<=?) maxUp maxDn e1 e2 e3

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> t -> Bool
propRoundedLatticeMeetIdempotent _ = 
    roundedIdempotent (<=?) minUp minDn

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (<=?) minUp minDn e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (<=?) minUp minDn e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (<=?) maxUp minUp maxDn minDn e1 e2 e3

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (<=?) maxUp minUp maxDn minDn e1 e2 e3)
    && 
    (roundedLeftDistributive  (<=?) maxUp minUp maxDn minDn e1 e2 e3)

testsRoundedLatticeDistributive :: 
    (PartialComparison t,
     RoundedLattice t,
     Arbitrary t, 
     ArbitraryOrderedTuple t,
     Eq t, 
     Show t) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRoundedLatticeDistributive (name, sample) maybeIllegalArg =
    testGroup (name ++ " (min,max) rounded") $
        (case maybeIllegalArg of 
            Nothing -> []
            Just (illegalArgName, illegalArg) -> 
                [testProperty (illegalArgName ++ " exception") 
                              (propRoundedLatticeIllegalArgException illegalArg)]) 
        ++
        [
         testProperty "Comparison compatible" (propRoundedLatticeComparisonCompatible sample)
        ,
         testProperty "join above" (propRoundedLatticeJoinAboveBoth sample)
        ,
         testProperty "meet below" (propRoundedLatticeMeetBelowBoth sample)
        ,
         testProperty "join idempotent" (propRoundedLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propRoundedLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propRoundedLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRoundedLatticeMeetAssocative sample)
        ,
         testProperty "distributive" (propRoundedLatticeDistributive sample)
        ]

--{-|
--    A type with directed-rounding lattice operations
--    that also supported in-place.
---}
--class (RoundedLattice t, CanBeMutable t) => RoundedLatticeMutable t where
--    {-| maxUpMutable e a b c means a := maxUp e b c; a can be the same as b and/or c -}
--    maxUpMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--    {-| maxDnMutable e a b c means a := maxDn e b c; a can be the same as b and/or c -}
--    maxDnMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--    {-| minUpMutable e a b c means a := minUp e b c; a can be the same as b and/or c -}
--    minUpMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--    {-| minDnMutable e a b c means a := minDn e b c; a can be the same as b and/or c -}
--    minDnMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--
--    -- TODO: add default implementations using read/write
--    